

resource "digitalocean_loadbalancer" "lb" {
  name   = "${var.cluster_name}-lb"
  region = var.do_region



  forwarding_rule {
    entry_port     = 443
    entry_protocol = "https"
    target_port    = 3000
    target_protocol = "http"

    certificate_name = digitalocean_certificate.lb_cert.name
    #tls_passthrough = true
  }

  healthcheck {
    port     = 3000
    protocol = "http"
    path     = "/"
  }

  droplet_tag = "${var.cluster_name}"

}


resource "digitalocean_domain" "domain" {
  name = var.domain_name
}

resource "digitalocean_record" "subdomain" {
  domain = digitalocean_domain.domain.id
  type   = "A"
  name   = "@"
  value  =  digitalocean_loadbalancer.lb.ip
}

resource "digitalocean_certificate" "lb_cert" {
  name   = "${var.cluster_name}-cert"
  type   = "lets_encrypt"
  domains = [digitalocean_domain.domain.name]


}

module "droplet-pm2-1" {
  source = "./modules/pm2-droplet"
  do_region = var.do_region
  name = "${var.cluster_name}-pm2-1"
  do_droplet_size = "s-1vcpu-1gb"
  tag = "${var.cluster_name}"
}

output "droplet-pm2-1-ipv4_address" {
  value = module.droplet-pm2-1.ipv4_address
}