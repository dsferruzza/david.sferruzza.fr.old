---
title: Gérer les erreurs avec l'aide du système de types de Scala !
author: David Sferruzza
lang: fr
tags: scala, fp, conf
description: Ma conférence à DevoxxFR 2016
published: 2016-05-10 16:10:00+02:00
---

Le 22 avril dernier, j'ai eu la chance de donner une conférence de 45 minutes à [DevoxxFR](https://devoxx.fr/) :

> **Gérer les erreurs avec l'aide du système de types de Scala !**
>
> Certaines fonctions de nos programmes peuvent échouer à calculer une valeur de retour. Plutôt que d'utiliser des exceptions, il est possible de tirer partie du système de types de Scala pour gérer les erreurs de manière plus fiable et lisible.
>
> Nous verrons comment utiliser des ADT (Algebraic Data Type) comme `Option`, `Try` et `Either`, ainsi que `disjunction` et `validation` de [Scalaz](https://github.com/scalaz/scalaz). On regardera aussi la bibliothèque [Rapture](http://rapture.io/) et ses "modes" qui offrent la possibilité *à l'appelant* de choisir de quelle manière il veut encapsuler les éventuelles erreurs.
>
> La gestion des erreurs est bien moins compliquée lorsque le compilateur peut nous aider !

Voilà la vidéo et les slides :

- Slides : <https://dsferruzza.github.io/conf-gestion-erreurs-en-scala/> ([dépôt GitHub](https://github.com/dsferruzza/conf-gestion-erreurs-en-scala))
- Vidéo : <https://www.youtube.com/watch?v=TwJQKrZ23Vs>

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/TwJQKrZ23Vs?rel=0" frameborder="0" allowfullscreen></iframe>

DevoxxFR 2016 c'était super, et j'espère revenir l'année prochaine !

*Merci à [Clément Delafargue](http://blog.clement.delafargue.name/) de m'avoir fait découvrir les modes de [Rapture](http://rapture.io/).*
