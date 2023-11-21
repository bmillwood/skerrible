resource "aws_lb_target_group" "skerrible" {
  name = "skerrible"
  port = var.internal_port
  protocol = "HTTP"
  target_type = "ip"
  vpc_id = aws_vpc.main.id
}

resource "aws_lb" "skerrible" {
  name = "skerrible"
  internal = false
  load_balancer_type = "application"
  subnets = [aws_subnet.a.id, aws_subnet.b.id]
  security_groups = [aws_security_group.load_balancer.id]
}

resource "aws_lb_listener" "skerrible_http" {
  load_balancer_arn = aws_lb.skerrible.arn
  port = 80
  protocol = "HTTP"
  default_action {
    type = "redirect"
    redirect {
      port = "443"
      protocol = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}

resource "aws_lb_listener" "skerrible_https" {
  load_balancer_arn = aws_lb.skerrible.arn
  port = 443
  protocol = "HTTPS"
  certificate_arn = var.certificate_arn
  ssl_policy = "ELBSecurityPolicy-2016-08"
  default_action {
    type = "forward"
    target_group_arn = aws_lb_target_group.skerrible.arn
  }
}
