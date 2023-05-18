pipeline {
    agent { label 'nixos' }
    stages {
        stage ('Cachix setup') {
            steps {
                cachixUse "nammayatri"
            }
        }
        stage ('Nix Build All') {
            steps {
                nixBuildAll ()
            }
        }
        stage ('Cachix push') {
            when {
                anyOf { branch 'main' }
            }
            steps {
                cachixPush "nammayatri"
            }
        }
    }
}
