# Instructions pour exécuter un programme OCaml

Ce référentiel contient un programme OCaml qui implémente un jeu. Voici comment exécuter le programme sur votre système.

## Prérequis

Assurez-vous d'avoir OCaml et Opam installés sur votre système. Si ce n'est pas le cas, vous pouvez les installer en suivant les instructions sur les sites officiels :

- [OCaml](https://ocaml.org/docs/install.html)
- [Opam](https://opam.ocaml.org/doc/Install.html)

## Installation des dépendances

Le programme utilise le package `graphics` pour l'interface graphique. Assurez-vous de l'installer en exécutant la commande suivante :

```bash
opam install graphics

ocamlbuild C4_mainT.native
ocamlbuild -package graphics C4_mainG.native
```
