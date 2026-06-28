Ce package rassemble des fonctions pour estimer et simuler des équations à corrections d'erreurs. 
Il permet de réaliser l'estimation dans laquelle la significativité de la force de rappel est évaluée à l'aide 
des simulations d'Ericsson MacKinnon. Des tests de normalité, autocorrelation, heteroscedasticité sont calculés.
Des graphiques représentant les résidus et le fit (statique) des équations estimées sont intégrés.
Il est également possible de réaliser des simulations dynamiques et de construire des tableaux comparant plusieurs spécifications.

La package peut être téléchargé à l'aide de la commande `pak::pak("ofce/EcmOfce")`

La fonction `EcmOfce::copy_examples()` permet de copier automatiquement un exemple d'estimation/simulation dynamique sur R 
et un .qmd avec un exemple de tableau dans le working directory
