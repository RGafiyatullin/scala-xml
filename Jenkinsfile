pipeline {
  agent {
    docker {
      image '1science/sbt'
    }
    
  }
  stages {
    stage('') {
      steps {
        sh 'sbt test'
      }
    }
  }
}