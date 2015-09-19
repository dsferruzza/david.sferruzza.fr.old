---
title: Un peu de programmation fonctionnelle en JavaScript !
author: David Sferruzza
lang: fr
tags: javascript, fp
description: Quelques concepts de programmation fonctionnelle appliqués à JavaScript
published: 2015-09-19 17:20:00+02:00
---

> Cet article est une très légère variante de celui que j'ai publié dans [24 jours de web 2014](http://www.24joursdeweb.fr/2014/un-peu-de-programmation-fonctionnelle-en-javascript/).
> Je l'ai aussi adapté en [présentation](https://github.com/dsferruzza/talk-programmation-fonctionnelle-en-js).

Il y a un peu moins de deux ans, j'ai découvert la programmation fonctionnelle.
Je me suis notamment renseigné sur un langage nommé **Haskell**, et je suis tombé sur un article qui énumérait des raisons pour lesquelles l'apprendre valait le coup.
Parmi elles, il y avait la promesse que, même si on n'avait pas l'occasion ou l'intention d'utiliser ce langage tous les jours, l'apprendre permettrait d'être meilleur dans les langages "classiques" (dits *impératifs*).
Un peu hésitant, j'ai commencé à apprendre et à jouer avec ce nouveau langage...

Au début, c'est déroutant.
On a l'impression de ne plus rien savoir faire : la programmation purement fonctionnelle est un paradigme assez différent de la programmation impérative que l'on pratique dans la plupart des langages populaires.
Mais avec un peu d'efforts, on commence à percevoir les avantages que ça apporte, et croyez-moi, ils sont vraiment intéressants !
Et savez-vous ce qui est encore plus appréciable ?
La promesse est vraie : il est possible, dans une certaine mesure, d'appliquer des concepts de programmation fonctionnelle dans des langages comme JavaScript, et ça permet de faire des programmes plus fiables et plus faciles à maintenir !

Mais vous n'êtes pas obligé de me croire sur parole.
Mon objectif avec cet article est de vous faire entrevoir le gain que l'on peut obtenir en appliquant des concepts de programmation fonctionnelle à des langages impératifs ; en l'occurrence, à JavaScript.

On va commencer en douceur avec quelques concepts théoriques, et puis on fera un peu de pratique en manipulant des tableaux en JavaScript.
Pas besoin d'être Tony Stark, si vous avez déjà fait un peu de JavaScript, et éventuellement vu les fonctions en mathématiques, tout ça devrait vous parler !


## La transparence référentielle

Avant toute chose, on va parler des fonctions, puisqu'elles sont au cœur de la programmation fonctionnelle.
Voici un exemple de fonction en JavaScript :

<!-- Une fonction est *une routine qui retourne une valeur*.
De manière générale, une fonction possède :

- un nom
- des paramètres d'entrée (chacun a un nom et un type)
- un type de sortie
- un bloc de code (le corps de la fonction), qui va pouvoir utiliser les valeurs fournies en entrée pour calculer une valeur de sortie

On va spécifier toutes ces informations lors de la **définition** de la fonction, et on pourra ensuite **appeler** la fonction pour obtenir la valeur de retour.
En JavaScript, cela se fait comme ça : -->

```javascript
// On définit une fonction "ajouterCinq" qui prend un nombre en paramètre,
// lui ajoute 5, et renvoie le résultat comme valeur de retour
function ajouterCinq(nombre) {
	return nombre + 5;
}

// On peut maintenant appeler cette fonction pour obtenir un résultat
// sans avoir à réécrire toute la logique de calcul à chaque fois

ajouterCinq(0);
// 5

ajouterCinq(8);
// 13
```

Les fonctions sont très importantes car elles permettent de regrouper les calculs similaires à un seul endroit dans le code, ce qui évite de se répéter et rend le code beaucoup plus lisible.
Elles permettent de découper un problème compliqué en plusieurs problèmes moins compliqués, ce qui est très souvent une bonne idée.

Étudions une autre fonction :

```javascript
function diviserParDeux(nombre) {
	var missile = lancerUnMissileThermonucleaire();
	fairePorterLeChapeauAuDrManhattan(missile);
	return nombre / 2;
}
```

Si on appelle `diviserParDeux(4)`, on obtiendra `2` en valeur de retour.
Mais on voit dans le code de la fonction que celle-ci va appeler d'autres fonctions qui vont visiblement modifier des choses qui sont à l'extérieur de la fonction `diviserParDeux`.
Ce qui est gênant, c'est qu'on n'a pas idée des effets que va avoir l'appel de cette fonction étant donné que cela n'apparait ni dans ces paramètres d'entrée, ni dans sa valeur de retour.
On dit que cette fonction comporte des **effets de bord**.

Notre fonction `ajouterCinq` n'a pas d'effets de bord : elle ne va pas accéder à des zones mémoire qui sont hors de son bloc de code.
On dit que c'est une **fonction pure**.
Et ce qui est pratique avec les fonctions pures, c'est qu'elles retournent toujours le même résultat lorsqu'on les appelle avec les mêmes arguments :

```javascript
ajouterCinq(-4) == ajouterCinq(-4)
// --> 1 == 1
// --> true

// Alors que cela ne fonctionne pas forcément avec une fonction
// qui a des effets de bord
var n = 0;
function ajouterAuCompteur(nombre) {
	n = n + nombre;
	// On accède à la variable "n" sans qu'elle ne soit passée en paramètre
	// et on la modifie en lui assignant une nouvelle valeur
	return n;
}

ajouterAuCompteur(1) == ajouterAuCompteur(1)
// --> 1 == 2
// --> false
```

Les fonctions pures permettent la **transparence référentielle** : le résultat du programme ne change pas si on remplace une expression par une expression de valeur équivalente.
Dans notre cas, comme on sait que `ajouterCinq(-4)` est strictement équivalent à `1` (car il n'y a pas d'effets de bords), on pourrait remplacer toutes les occurrences de `ajouterCinq(-4)` par `1` dans notre programme sans que ça ne change son résultat.

Si on remplaçait toutes les occurrences de `diviserParDeux(6)` par `3`, on obtiendrait un programme différent car un appel à la fonction `diviserParDeux(6)` a des effets de bord que n'a pas la valeur `3`.

La transparence référentielle est très intéressante car elle rend votre programme facilement prévisible par le lecteur : il devient possible de raisonner dessus comme si c'était une équation, et non plus une séquence d'instructions arbitraires !
Il y a même un terme pour désigner ce concept : *equational reasoning*.

En s'efforçant de construire son programme avec un maximum de fonctions pures, on va alors se retrouver à isoler les fonctions qui ont des effets de bord, ce qui offre plusieurs avantages :

- on sait quelles fonctions ont des effets de bord, ce qui permet d'être prudent lorsqu'on les manipule
- le cœur du programme, la logique métier, est exprimée avec des fonctions pures qui sont fiables, sans surprises, et aisément testables

*Ne vous inquiétez pas si c'est encore un peu flou ; la prochaine partie sera moins théorique et plus concrète !*


## Les fonctions d'ordre supérieur

Dans la partie précédente, je suis rapidement revenu sur ce qu'est une fonction.
Ce que je n'ai pas dit, c'est qu'en JavaScript, les fonctions sont des **objets de première classe**, au même titre que les chaines de caractères, les nombres, et bien d'autres...

Cela veut dire qu'une fonction peut :

- être expressible comme une valeur anonyme littérale
- être affectée à des variables ou des structures de données
- avoir une identité intrinsèque
- être comparable pour l'égalité ou l'identité avec d'autres entités
- être passée comme paramètre à une procédure ou une fonction
- être retournée par une procédure ou une fonction
- être constructible lors de l'exécution

Suivant les langages, les fonctions ne sont pas toujours des objets de première classe : en C, par exemple, ce n'est pas le cas.
Mais quand c'est le cas, cela permet des constructions intéressantes : les **fonctions d'ordre supérieur**.

Une fonction d’ordre supérieur est une fonction qui accepte au moins une autre fonction en paramètre, et/ou qui retourne une fonction en résultat.
C'est tout !
Voyons un exemple :

```javascript
// On a une fonction qui accepte un nombre et renvoie ce nombre multiplié par 2
function multiplierParDeux(nombre) {
	return nombre * 2;
}

// On a une autre fonction qui accepte un paramètre quelconque, va l'afficher
// et le renvoyer en valeur de retour
function afficher(x) {
	console.log(x); // <-- Ceci est un effet de bord
	return x;
}

// Maintenant imaginons qu'on souhaite appliquer multiplierParDeux() sur un nombre,
// et ensuite afficher() sur le résultat
afficher(multiplierParDeux(5));
// 10
// --> 10

// Cela fonctionne, mais ce n'est pas pratique si on doit s'en servir beaucoup.
// Il faudrait avoir une fonction qui fasse les 2 actions directement,
// qu'on pourrait appeler afficherDouble()

// On pourrait faire comme ça
function afficherDouble(nombre) {
	return afficher(multiplierParDeux(nombre));
}

// Ou on pourrait créer une fonction plus générale
// qui pourrait être réutilisée partout !
function compose(f, g) {
	return function(x) {
		return f(g(x));
	};
}
// compose() accepte deux fonctions (f et g) en paramètre, et renvoie en retour
// une fonction (qui est équivalente à la composition de f et g)
// C'est donc une fonction d'ordre supérieur !

// Ainsi
var afficherDoubleBis = compose(afficher, multiplierParDeux);
// On indique "afficher" et pas "afficher()" car on veut désigner
// la fonction en elle-même, pas l'appeler

afficherDouble(5) == afficherDoubleBis(5)
// --> true
```

Cet exemple, bien qu'un peu abstrait, laisse entrevoir l'intérêt des fonctions d'ordre supérieur : on peut facilement créer des fonctions très générales et les rendre plus spécifiques en leur passant d'autres fonctions en paramètre.
Cela permet de bien séparer les différentes tâches effectuées par nos fonctions.
C'est justement en ayant en tête cette idée de séparer nos traitements en petites fonctions modulaires qu'on va regarder un exemple plus concret : la manipulation des tableaux.

### Les tableaux

En JavaScript, un tableau peut être vu comme *une liste ordonnée de valeurs*.
Une valeur peut être un nombre, un objet, une fonction, ... n'importe quel objet de première classe !
Rien ne l'empêche en JavaScript mais, quand on est bien élevé, on évite de faire des tableaux contenant plusieurs types de valeurs :

```javascript
var tableau = [1, 2, 3, 4]; // Bien
var tableau = [1, "deux", 3, { valeur: 4 }]; // Mal
```

C'est une mauvaise pratique car il devient difficile de raisonner sur les fonctions qui vont manipuler une telle liste.
Une solution serait que chaque élément de la liste soit un élément d'une structure qui permette d'exprimer tous les cas qu'on a besoin d'exprimer ; un objet pourrait faire l'affaire ici.

Les tableaux sont une structure de données extrêmement utile et répandue, et il y a un certain nombre d'opérations qu'on est amené à faire très souvent.
Par exemple, on souhaite souvent transformer chaque élément d'un tableau en quelque chose d'autre.

```javascript
// On a une liste d'objets qui représentent des villes
// (on peut dire qu'on l'a obtenue grâce à un appel HTTP vers une API externe)
var villes = [
	{ nom: "Nantes", departement: 44, presDeLaMer: false },
	{ nom: "Dunkerque", departement: 59, presDeLaMer: true },
	{ nom: "Paris", departement: 75, presDeLaMer: false },
];

// On veut obtenir une liste de chaines de type "ville (departement)"
function rendreVillesAffichables(villes) {
	for (var i = 0; i < villes.length; i++) {
		villes[i] = villes[i].nom + " (" + villes[i].departement + ")";
	}
	return villes;
}

rendreVillesAffichables(villes);
// --> ["Nantes (44)", "Dunkerque (59)", "Paris (75)"]
```

Le résultat est correct, mais il y a plusieurs problèmes avec cette fonction `rendreVillesAffichables` :

1. Elle modifie le tableau original, ce qui fait qu'elle n'est pas pure : en JavaScript, quand on copie une variable qui contient un tableau ou un objet, seule la référence vers les données est copiée, ce qui fait qu'une modification sur les cases du tableau ou les propriétés de l'objet sera visible depuis la variable originale **et** depuis la copie.
2. On a mélangé deux comportements dans notre fonction :
	- parcourir le tableau : c'est quelque chose de très courant, peu importe ce que contient le tableau
	- effectuer la transformation qu'on souhaite, pour une case donnée : c'est un comportement spécifique, et donc métier, qui se retrouve noyé dans la logique d'itération sur le tableau

Certes, l'exemple ici est plutôt simple et concis, mais on peut quand même améliorer ça !

### map

On pourrait imaginer une fonction d'ordre supérieur qui permette d'abstraire le parcours du tableau.
Cette fonction accepterait deux paramètres : le tableau à transformer, et une fonction effectuant la transformation sur un seul élément du tableau.
Essayons de coder ça !

```javascript
function transformerTableau(tableau, transformation) {
	// On crée un nouveau tableau qu'on va remplir
	// plutôt que de modifier celui fourni en paramètre
	var nouveauTableau = [];
	for (var i = 0; i < tableau.length; i++) {
		nouveauTableau[i] = transformation(tableau[i]);
	}
	return nouveauTableau;
}

// Ainsi, notre fonction rendreVillesAffichables devient :
function rendreVillesAffichables(villes) {
	function rendreUneSeuleVilleAffichable(ville) {
		return ville.nom + " (" + ville.departement + ")";
	}
	return transformerTableau(villes, rendreUneSeuleVilleAffichable);
}

rendreVillesAffichables(villes);
// --> ["Nantes (44)", "Dunkerque (59)", "Paris (75)"]
```

C'est quand même bien plus clair !
La fonction `transformerTableau` n'embarque aucune logique métier et fonctionne avec n'importe quel tableau, et la fonction `rendreUneSeuleVilleAffichable`, qui contient la logique métier de la transformation, est pure, et peut donc être testée très facilement.

Et le plus beau dans tout ça, c'est que notre fonction `transformerTableau` est tellement utile et générique qu'elle est déjà présente dans l'API des tableaux de JavaScript !
Dites bonjour à [Array.map](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Array/map) !

```javascript
// La même chose que précédemment, mais en utilisant Array.map
function rendreVillesAffichables(villes) {
	function rendreUneSeuleVilleAffichable(ville) {
		return ville.nom + " (" + ville.departement + ")";
	}
	return villes.map(rendreUneSeuleVilleAffichable);
}

rendreVillesAffichables(villes);
// --> ["Nantes (44)", "Dunkerque (59)", "Paris (75)"]
```

La fonction `map` est présente sur tous les tableaux JavaScript.
Quand on l'appelle, en lui passant en argument une fonction qui accepte un élément du tableau et renvoie le nouvel élément, elle va appliquer cette fonction à chaque case du tableau et renvoyer le résultat sous forme d'un nouveau tableau.

Le tableau de retour a **exactement** le même nombre d'éléments (dans le même ordre) que le tableau d'origine : `map` ne modifie pas la structure (le tableau en lui-même) mais son contenu (les éléments).
*Il est tout de même possible en JavaScript de modifier la structure du tableau (en ajoutant ou supprimant des éléments), mais c'est une très mauvaise idée (nous verrons bientôt une solution plus sympathique pour faire ça).*

<!-- Voici plusieurs propriétés intéressantes de `map` :

```javascript
// La fonction identité :
// Elle renvoie l'argument qu'on lui a fourni sans le modifier
function id(x) {
	return x;
}

// Pour tout tableau
// tableau.map(id) == tableau

// Soient f et g deux fonctions de transformation
// Pour tout tableau
// tableau.map(f).map(g) == tableau.map(compose(f, g))
``` -->

### filter

Un autre besoin très courant lorsqu'on manipule des tableaux est de vouloir supprimer certains éléments suivant un critère spécifique.
Encore une fois, nous sommes chanceux, il existe une fonction dans l'API des tableaux JavaScript qui fait exactement ça : [Array.filter](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Array/filter).

La fonction `filter` est, comme `map`, présente sur tous les tableaux JavaScript.
Elle a besoin qu'on lui passe une fonction de prédicat, c'est à dire une fonction qui prend un élément comme paramètre et qui renvoie `true` ou `false`.

Par exemple, en partant de notre tableau de villes, on souhaite ne conserver que les villes qui sont près de la mer :

```javascript
// Cette fois-ci, plutôt que de créer une fonction nommée,
// je passe une fonction anonyme
villes.filter(function(item) {
	// Si la ville est près de la mer, on renvoie true, sinon false
	return item.presDeLaMer;
});
// --> [ { nom: "Dunkerque", departement: 59, presDeLaMer: true } ]
```

Plutôt chouette, non ?
On peut même *chainer* `filter` et `map` pour ne garder que les villes qui ne sont pas près de la mer et les afficher de manière élégante :

```javascript
villes.filter(function(item) {
	return !item.presDeLaMer;
}).map(function(item) {	// Comme villes.filter renvoie un tableau,
			// on peut appeler map dessus !
	return item.nom + " (" + item.departement + ")";
});
// --> ["Nantes (44)", "Paris (75)"]
```

### reduce

Enfin, on a parfois besoin de transformer un tableau en quelque chose d'autre.
Autrement dit, on ne veut pas toujours conserver intacte la structure qui contient nos données (comme c'est le cas avec `map` et `filter`).
Par exemple, on a un tableau qui contient des objets ayant tous une propriété `age` et on souhaite calculer la somme de tous les âges.

La fonction [Array.reduce](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Array/reduce) (également appelée **fold** suivant les langages) est faite pour ça !
Elle a besoin des paramètres suivants : (*attention ils ne sont pas dans le bon ordre, mais c'est plus simple à comprendre comme ça*)

- un tableau (`reduce` s'appelle directement sur le tableau, comme `map` et `filter`)
- une valeur initiale (elle sera renvoyée si le tableau est vide, notamment)
- une fonction qui accepte deux paramètres :
	- la valeur actuelle de l'**accumulateur** (pour la 1ère itération, c'est la valeur initiale, et pour les suivantes, ce sera la valeur renvoyée par la fonction lors de l'itération précédente)
	- la valeur de la case du tableau courante

```javascript
var personnes = [
	{ nom: "Bruce", age: 30 },
	{ nom: "Tony", age: 35 },
	{ nom: "Peter", age: 26 },
];

personnes.reduce(function(acc, cur) {
	// On ajoute l'âge courant à la valeur actuelle de la somme
	return acc + cur.age;
}, 0);
// --> 91
```

`reduce` est très pratique car elle permet de plier (*fold*) un tableau, ce qui est nécessaire relativement souvent.
Elle est plus générale que `map` et `filter` ; on peut d'ailleurs les exprimer tous les deux en terme de `reduce` :

```javascript
function map(tableau, transformation) {
	return tableau.reduce(function(acc, cur) {
		acc.push(transformation(cur));
		return acc;
	}, []);
}

function filter(tableau, predicat) {
	return tableau.reduce(function(acc, cur) {
		if (predicat(cur)) acc.push(cur);
		return acc;
	}, []);
}
```
-------

Pour résumer, si vous avez un tableau et que vous voulez :

- appliquer une transformation sur chacune de ses cases en conservant leur ordre/nombre : `map`
- supprimer certaines cases en conservant l'ordre des autres : `filter`
- le parcourir pour construire une nouvelle structure de données : `reduce`

Ces fonctions permettent de manipuler des tableaux de manière pure, avec tous les avantages que ça implique.
Je vous encourage à les utiliser, ou à définir vous-même les fonctions d'ordre supérieur dont vous avez besoin !


## Conclusion

Tout le code ne peut pas être pur, surtout en JavaScript.
Mais plus il l'est, plus il sera clair, réutilisable, fiable et vous fera vous poser les bonnes questions.
Écrire du code qui fonctionne ne suffit pas. La transparence référentielle et les fonctions d'ordre supérieur sont des outils qui vous facilitent grandement la vie lorsque vous réfléchissez sur votre programme.
Elles augmentent la qualité du code que vous écrivez.
Le prix à payer est modique par rapport au gain !

Il existe d'autres concepts très intéressants en programmation fonctionnelle.
On pourrait citer l'évaluation paresseuse, l'immuabilité, ou les systèmes de types avancés.
Ces derniers permettent notamment quelque chose de similaire à l'analyse dimensionnelle en physique : le langage va nous empêcher d'exprimer des incohérences (on ne va pas additionner par erreur une valeur en `km` avec une valeur en `miles`, par exemple).

J'espère que vous avez apprécié cet article !
Voici quelques ressources si vous voulez approfondir un peu ces sujets :

- [Learn You A Haskell For Great Good](http://learnyouahaskell.com/) : LE livre pour débuter Haskell ; il est très accessible ; disponible en papier ou en ligne ([traduction FR non officielle](http://lyah.haskell.fr/))
- [JavaScript Alongé](https://leanpub.com/javascript-allonge) : un livre avancé sur JavaScript ; il explique et met en place certains concepts que nous avons survolés ici ; disponible en version dématérialisée ou en ligne
- [Lo-Dash](https://lodash.com/) : une bibliothèque JavaScript qui propose notamment des fonctions d'ordre supérieur très intéressantes pour manipuler les collections
- [Bacon.js](https://baconjs.github.io/) : une bibliothèque JavaScript qui permet de faire de la programmation fonctionnelle [réactive](https://fr.wikipedia.org/wiki/Programmation_r%C3%A9active)
- [Is your programming language unreasonable?](http://fsharpforfunandprofit.com/posts/is-your-language-unreasonable/) *or, why predictability is important*

*Merci à [Clément Delafargue](http://blog.clement.delafargue.name/), [Francois-Guillaume Ribreau](http://fgribreau.com/articles/voyage-au-coeur-de-javascript.html), [Mathieu Le Gac](https://twitter.com/mateoelgaco), [Elisabeth Hamel](http://elisabeth-hamel.fr/) et [Thibault Mahé](http://tibomahe.com/) pour leurs relectures !*
