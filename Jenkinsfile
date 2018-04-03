pipeline {
  agent {
    docker {
      image '1science/sbt'
    }
    
  }
  stages {
    stage('error') {
      steps {
        sh 'sbt test'
      }
    }
  }
  environment {
    DOCKER_HOST = 'tcp://docker-proxy:2375'
  }
}