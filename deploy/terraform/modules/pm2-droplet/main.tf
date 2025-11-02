resource "digitalocean_ssh_key" "me" {
  name       = "my-local-key"
  public_key = file(pathexpand("~/.ssh/id_rsa.pub"))
}

resource "digitalocean_droplet" "droplet" {
  image    = "ubuntu-24-04-x64"
  name     = var.name
  region   = var.do_region
  size     = var.do_droplet_size

  ssh_keys = [digitalocean_ssh_key.me.fingerprint]

  connection {
    user = "root"
    type = "ssh"
    private_key = "${file("~/.ssh/id_rsa")}"
    timeout = "2m"

    host = self.ipv4_address
  }

  provisioner "remote-exec" {
    inline = [
    <<-EOT
      apt-get update -y
      apt-get install -y unzip build-essential python3 curl htop npm
      curl -fsSL https://bun.sh/install | bash
      curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
      # cat >> ~/.bashrc << 'EOF'
      # export NVM_DIR="$HOME/.nvm"
      # [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
      # [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
      # EOF
      nvm install --lts
      npm install -g npm pm2
    EOT
    ]
  }

  tags = [var.tag, "pm2"]

}
