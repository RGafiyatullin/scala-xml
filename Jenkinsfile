pipeline {
  agent {
    docker {
      image 'sbt:dev'
    }
    
  }
  stages {
    stage('sbt test') {
      steps {
        sh 'sbt test'
      }
    }
  }
}