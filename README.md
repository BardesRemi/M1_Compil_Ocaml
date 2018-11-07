# Rendu numéro 3 - Projet Compilation - Rémi Bardes, Nicolas Devatine

10/11/18 -> 11/07/18


## Travail effectué :

### Sujet 3.1 :

* Travail attendu effectué en totalité
* Extensions : Bornes des tableaux + Tableaux à n dimensions
	             
### Sujet 3 .2 :
* Travail attendu effectué en totalité
* Extensions : Entrelacer les déclarations + Champs immuables

## Travail entamé mais non fonctionnel : 
rien à notre connaissance

## Travail emprunté : 
Alban nous a donné des conseilles pour l'implémentation des matrices. Sans nous donner de codes, il nous a orienté sur la gestion des accés aux locations générés par les tableaux de dimension n.

# Rendu numéro 2 - Projet Compilation - Rémi Bardes, Nicolas Devatine

09/28/18 -> 10/10/18

Etant donné que nous n'avons pas forcé la première instructions du "FOR" à être une affectation simple, ça peut donc être n'importe quelle instruction. Cela génère un conflit avec notre implémentation de l'extension "x,y := e1,e2;" car les firgules présentes dans cette nouvelles instructions déclenche un conflit avec les virgules du FOR.
Nous avons donc pris la décision de changer la syntaxe du FOR en remplaçant les virgules par des points virgules.
	
## Travail effectué :

### Sujet 2.1 :

* Travail attendu effectué en totalité
* Extensions : Messages d'erreur, Commentaires longs, Préprocesseur (Macros constantes, Macros avec arguments, Macros contenant d'autres macros)
	
### Sujet 2 .2 :
* Travail attendu effectué en totalité
* Extensions : Sucre syntaxique sauf affectation simultanée (incomplète)

## Travail entamé mais non fonctionnel :

Dans l'extension "sucre syntaxique" pour l'affectation simultanée le cas x, y := y, x rend : x := y / y := x (qui vaut y)

## Travail emprunté :

Aucun

# Rendu numéro 1 - Projet Compilation - Rémi Bardes, Nicolas Devatine

09/14/18 -> 09/26/18
	
## Travail effectué :

### Sujet 1.1 :

* Travail attendu effectué en totalité
* Extensions : break/continue, boucle for
	
### Sujet 1.2 :
* Travail attendu effectué en totalité
* Extensions : instructions combinées, simplification des expressions arithmétiques constantes, optimisation de l'utilisation de la pile

## Travail entamé mais non fonctionnel :

Aucun

## Travail emprunté :

Aucun