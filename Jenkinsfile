pipeline {
  agent {
    docker {
      image 'registry.wdo.io/r_hafiyatulin/sbt-wg-artifactory:dev'
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