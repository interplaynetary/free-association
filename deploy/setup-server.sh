#!/bin/bash
# Initial server setup script for DigitalOcean droplet
# Run this once on your droplet to set up the environment

set -e

echo "🚀 Setting up Free Association backend on DigitalOcean..."

# Update system
echo "📦 Updating system packages..."
sudo apt-get update
sudo apt-get upgrade -y

# Install required packages
echo "📦 Installing required packages..."
sudo apt-get install -y curl git rsync nginx certbot python3-certbot-nginx

# Install Bun
if ! command -v bun &> /dev/null; then
    echo "📦 Installing Bun..."
    curl -fsSL https://bun.sh/install | bash
    export BUN_INSTALL="$HOME/.bun"
    export PATH="$BUN_INSTALL/bin:$PATH"
    echo 'export BUN_INSTALL="$HOME/.bun"' >> ~/.bashrc
    echo 'export PATH="$BUN_INSTALL/bin:$PATH"' >> ~/.bashrc
fi

# Create application directory
echo "📁 Creating application directory..."
sudo mkdir -p /var/www/free-association
sudo chown -R $USER:$USER /var/www/free-association

# Create data directories
echo "📁 Creating data directories..."
mkdir -p /var/www/free-association/gun-data
mkdir -p /var/www/free-association/holster-data
mkdir -p /var/backups/free-association-data

# Create log directory
echo "📁 Creating log directory..."
sudo mkdir -p /var/log/free-association
sudo chown -R $USER:$USER /var/log/free-association

# Create .env file template
echo "📝 Creating .env template..."
cat > /var/www/free-association/.env << 'EOF'
# Free Association - Production Environment
NODE_ENV=production
PORT=3000
APP_URL=https://your-domain.com

# Generate these secrets!
JWT_SECRET=CHANGE_ME_$(openssl rand -base64 32)
MASTER_API_KEY=CHANGE_ME_$(openssl rand -hex 32)

# OpenRouter API Keys
OPENROUTER_KEYS=your-keys-here
OPENROUTER_BASE_URL=https://openrouter.ai/api/v1

# Gun Relay
GUN_RELAY_HOST=0.0.0.0
GUN_RELAY_PORT=8765
GUN_RELAY_STORE=true
GUN_RELAY_PATH=gun-data
GUN_RELAY_SHOW_QR=false

# Holster Relay
HOLSTER_RELAY_HOST=0.0.0.0
HOLSTER_RELAY_PORT=8766
HOLSTER_RELAY_STORAGE=true
HOLSTER_RELAY_STORAGE_PATH=./holster-data
HOLSTER_MAX_CONNECTIONS=500

# CORS (your GitHub Pages domain)
ALLOWED_ORIGINS=https://YOUR_USERNAME.github.io,https://your-custom-domain.com
EOF

echo "⚠️  IMPORTANT: Edit /var/www/free-association/.env and set your secrets!"
echo ""

# Setup Nginx reverse proxy
echo "🌐 Setting up Nginx..."
sudo tee /etc/nginx/sites-available/free-association > /dev/null << 'EOF'
# Free Association API Server
server {
    listen 80;
    server_name api.your-domain.com;  # Change this!

    # Rate limiting
    limit_req_zone $binary_remote_addr zone=api_limit:10m rate=10r/s;
    limit_req zone=api_limit burst=20 nodelay;

    # Logging
    access_log /var/log/nginx/free-association-access.log;
    error_log /var/log/nginx/free-association-error.log;

    # API routes
    location /api/ {
        proxy_pass http://localhost:3000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_cache_bypass $http_upgrade;
        
        # CORS headers
        add_header Access-Control-Allow-Origin $http_origin always;
        add_header Access-Control-Allow-Methods "GET, POST, PUT, DELETE, OPTIONS" always;
        add_header Access-Control-Allow-Headers "Authorization, Content-Type" always;
        add_header Access-Control-Allow-Credentials "true" always;
        
        if ($request_method = OPTIONS) {
            return 204;
        }
    }

    # Gun relay WebSocket
    location /gun {
        proxy_pass http://localhost:8765;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "Upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 86400;
    }

    # Holster relay WebSocket
    location /holster {
        proxy_pass http://localhost:8766;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "Upgrade";
        proxy_set_header Host $host;
        proxy_read_timeout 86400;
    }
}
EOF

# Enable site
sudo ln -sf /etc/nginx/sites-available/free-association /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx

# Setup systemd service (user needs to update it first)
echo "⚙️  Systemd service file created at: deploy/free-association.service"
echo "   You'll need to update the User and ExecStart paths, then:"
echo "   sudo cp deploy/free-association.service /etc/systemd/system/"
echo "   sudo systemctl enable free-association"
echo ""

# Setup firewall
echo "🔥 Configuring firewall..."
sudo ufw allow 22/tcp
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw allow 3000/tcp  # SvelteKit server
sudo ufw allow 8765/tcp  # Gun relay
sudo ufw allow 8766/tcp  # Holster relay
sudo ufw --force enable

# Setup automatic backups
echo "💾 Setting up daily backups..."
cat > /tmp/backup-free-association.sh << 'EOF'
#!/bin/bash
# Daily backup script for Free Association data

BACKUP_DIR="/var/backups/free-association-data"
DATE=$(date +%Y%m%d_%H%M%S)
RETENTION_DAYS=7

# Backup Gun data
if [ -d "/var/www/free-association/gun-data" ]; then
    tar -czf "$BACKUP_DIR/gun-data-$DATE.tar.gz" -C /var/www/free-association gun-data
fi

# Backup Holster data
if [ -d "/var/www/free-association/holster-data" ]; then
    tar -czf "$BACKUP_DIR/holster-data-$DATE.tar.gz" -C /var/www/free-association holster-data
fi

# Remove old backups
find "$BACKUP_DIR" -name "*.tar.gz" -mtime +$RETENTION_DAYS -delete

echo "Backup completed: $DATE"
EOF

sudo mv /tmp/backup-free-association.sh /usr/local/bin/
sudo chmod +x /usr/local/bin/backup-free-association.sh

# Add to crontab
(crontab -l 2>/dev/null; echo "0 2 * * * /usr/local/bin/backup-free-association.sh") | crontab -

echo ""
echo "✅ Server setup complete!"
echo ""
echo "📋 Next steps:"
echo "1. Edit /var/www/free-association/.env with your secrets"
echo "2. Update deploy/free-association.service with your user"
echo "3. Update Nginx config with your domain"
echo "4. Set up SSL: sudo certbot --nginx -d api.your-domain.com"
echo "5. Add GitHub secrets to your repository:"
echo "   - DROPLET_HOST: your server IP or domain"
echo "   - DROPLET_USER: your SSH username"
echo "   - DROPLET_SSH_KEY: your private SSH key"
echo "6. Push to GitHub to trigger deployment!"
echo ""
echo "🔐 Generate secrets:"
echo "   JWT_SECRET: openssl rand -base64 32"
echo "   MASTER_API_KEY: openssl rand -hex 32"
echo ""

