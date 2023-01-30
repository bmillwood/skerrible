resource "aws_vpc" "main" {
  cidr_block = "10.0.0.0/16"
}

# Application load balancers insist on having two different AZs
data "aws_availability_zones" "available" {
  state = "available"
}

resource "aws_subnet" "a" {
  vpc_id = aws_vpc.main.id
  map_public_ip_on_launch = true
  cidr_block = "10.0.0.0/24"
  availability_zone = data.aws_availability_zones.available.names[0]
}

resource "aws_subnet" "b" {
  vpc_id = aws_vpc.main.id
  map_public_ip_on_launch = true
  cidr_block = "10.0.1.0/24"
  availability_zone = data.aws_availability_zones.available.names[1]
}

resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id
}

resource "aws_route_table" "main" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }
}

resource "aws_route_table_association" "a" {
  subnet_id = aws_subnet.a.id
  route_table_id = aws_route_table.main.id
}

resource "aws_route_table_association" "b" {
  subnet_id = aws_subnet.b.id
  route_table_id = aws_route_table.main.id
}

resource "aws_security_group" "allow_outgoing_https" {
  vpc_id = aws_vpc.main.id
  name = "allow_outgoing_https"
  egress {
    protocol = "tcp"
    from_port = 443
    to_port = 443
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "allow_skerrible" {
  vpc_id = aws_vpc.main.id
  name = "allow_skerrible"
  ingress {
    from_port = var.internal_port
    to_port = var.internal_port
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "load_balancer" {
  vpc_id = aws_vpc.main.id
  name = "load_balancer"
  ingress {
    from_port = 80
    to_port = 80
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  ingress {
    from_port = 443
    to_port = 443
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
  egress {
    protocol = "tcp"
    from_port = var.internal_port
    to_port = var.internal_port
    cidr_blocks = [
      aws_subnet.a.cidr_block,
      aws_subnet.b.cidr_block,
    ]
  }
}
