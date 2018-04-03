pipeline {
  agent {
    docker {
      image '1science/sbt'
    }
    
  }
  stages {
    stage('error') {
      steps {
        sh 'env'
        sh 'docker ps -a'
      }
    }
  }
  environment {
    DOCKER_HOST = 'tcp://docker-proxy:2375'
  }
}