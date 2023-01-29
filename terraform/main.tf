terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.0"
    }
  }
}

data "aws_region" "current" {
}

resource "aws_ecr_repository" "skerrible" {
  name = "skerrible"
  image_tag_mutability = "IMMUTABLE"
}

resource "aws_iam_role" "skerrible" {
  name = "skerrible"
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = "sts:AssumeRole"
        Effect = "Allow"
        Principal = {
          Service = "ecs-tasks.amazonaws.com"
        }
      }
    ]
  })
}

resource "aws_iam_role_policy_attachment" "skerrible_execution" {
  role = aws_iam_role.skerrible.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

resource "aws_cloudwatch_log_group" "skerrible" {
  name = "skerrible"
  retention_in_days = 3
}

resource "aws_ecs_task_definition" "skerrible" {
  family = "skerrible"
  requires_compatibilities = ["FARGATE"]
  execution_role_arn = aws_iam_role.skerrible.arn
  network_mode = "awsvpc"
  cpu = 256
  memory = 512
  container_definitions = jsonencode([
    {
      name = "skerrible"
      image = "${aws_ecr_repository.skerrible.repository_url}:${var.image_tag}"
      logConfiguration = {
        logDriver = "awslogs"
        options = {
          "awslogs-region" = data.aws_region.current.name
          "awslogs-group" = aws_cloudwatch_log_group.skerrible.name
          "awslogs-stream-prefix" = "skerrible"
        }
      }
      environment = [
        { name = "HOST", value = "0.0.0.0" },
      ]
      portMappings = [
        { containerPort = var.internal_port },
      ]
    },
  ])
}

resource "aws_lb_target_group" "skerrible" {
  name = "skerrible"
  port = var.internal_port
  protocol = "TCP"
  target_type = "ip"
  vpc_id = aws_vpc.main.id
}

resource "aws_lb" "skerrible" {
  name = "skerrible"
  internal = false
  load_balancer_type = "network"
  subnets = [aws_subnet.main.id]
}

resource "aws_lb_listener" "skerrible" {
  load_balancer_arn = aws_lb.skerrible.arn
  port = 80
  protocol = "TCP"
  default_action {
    type = "forward"
    target_group_arn = aws_lb_target_group.skerrible.arn
  }
}

resource "aws_ecs_cluster" "skerrible" {
  name = "skerrible"
}

resource "aws_ecs_service" "skerrible" {
  name = "skerrible"
  cluster = aws_ecs_cluster.skerrible.id
  launch_type = "FARGATE"
  task_definition = aws_ecs_task_definition.skerrible.arn
  network_configuration {
    subnets = [aws_subnet.main.id]
    security_groups = [
      aws_security_group.allow_outgoing_https.id,
      aws_security_group.allow_skerrible.id,
    ]
    assign_public_ip = true
  }
  load_balancer {
    target_group_arn = aws_lb_target_group.skerrible.arn
    container_name = "skerrible"
    container_port = var.internal_port
  }
  desired_count = var.desired_count
}
