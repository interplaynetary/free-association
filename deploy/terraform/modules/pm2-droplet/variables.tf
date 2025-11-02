variable "name" {
  description = "Name of cluster"
  type        = string
  default     = "talos-do"
}

variable "do_region" {
  description = "DO region to use"
  type        = string
  default     = "nyc3"
}

variable "do_droplet_size" {
  description = "DO plan to use for worker nodes"
  type        = string
  default     = "s-1vcpu-1gb" #"s-2vcpu-4gb"
}

variable "tag" {
  description = "tag for the lb to find"
  type        = string

}

