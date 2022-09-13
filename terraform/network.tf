resource "aws_vpc" "main" {
  cidr_block = "10.0.0.0/16"
}

resource "aws_subnet" "main" {
  vpc_id = aws_vpc.main.id
  map_public_ip_on_launch = true
  cidr_block = aws_vpc.main.cidr_block
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

resource "aws_route_table_association" "main" {
  subnet_id = aws_subnet.main.id
  route_table_id = aws_route_table.main.id
}

resource "aws_security_group" "allow_outgoing" {
  vpc_id = aws_vpc.main.id
  name = "allow_outgoing"
  egress {
    protocol = "-1"
    from_port = 0
    to_port = 0
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "allow_skerrible" {
  vpc_id = aws_vpc.main.id
  name = "allow_skerrible"
  ingress {
    from_port = 4170
    to_port = 4170
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}
