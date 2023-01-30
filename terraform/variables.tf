variable "desired_count" {
  type = number
}

variable "internal_port" {
  type = number
  default = 4170
}

variable "image_tag" {
  type = string
}

variable "certificate_arn" {
  type = string
}
