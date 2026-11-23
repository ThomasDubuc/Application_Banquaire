# Banking System — Application bancaire en COBOL

Banking System est un projet personnel que j'ai commencé en septembre 2026 pour apprendre le COBOL de façon autonome. Il s'agit d'une application en console qui simule la gestion de comptes bancaires et quelques opérations courantes.

Mon objectif est de me familiariser avec le langage COBOL en développant un projet concret, puis de le faire évoluer au fur et à mesure de mon apprentissage.

## Fonctionnalités

- Créer un compte bancaire avec un numéro de compte et un solde initial de 0 $.
- Afficher les comptes enregistrés et leurs soldes.
- Rechercher un compte à partir de son numéro.
- Effectuer un dépôt ou un retrait.
- Effectuer un virement entre deux comptes.
- Vérifier les saisies et refuser certaines opérations invalides, notamment un retrait supérieur au solde disponible.

L'application est pour le moment limitée à cinq comptes. Il s'agit d'un projet d'apprentissage, et non d'un logiciel destiné à gérer de véritables comptes bancaires.

## Technologies utilisées

- **Langage :** COBOL
- **Compilateur :** GnuCOBOL
- **Exécution :** application en console
- **Gestion du code :** Git et GitHub

## Compilation et exécution

Il faut d'abord installer [GnuCOBOL](https://gnucobol.sourceforge.io/) et vérifier que la commande `cobc` est accessible dans le terminal.

Cloner le dépôt :

```bash
git clone https://github.com/ThomasDubuc/Application_Banquaire.git
cd Application_Banquaire
```

Depuis la racine du dépôt `Application_Banquaire`, compiler le fichier `Appli_Banquaire/src/main.cob` :

```bash
cobc -x -o BankingSystem Appli_Banquaire/src/main.cob
```

Sous Windows, lancer le programme avec :

```powershell
.\BankingSystem.exe
```

Le menu permet ensuite de choisir les opérations à effectuer.

## État du projet

La gestion des comptes et les opérations de dépôt, de retrait et de virement ont été développées et testées manuellement.

**En cours :** ajout de la sauvegarde et du chargement des comptes à l'aide de fichiers COBOL. La persistance des données n'est donc pas encore considérée comme terminée.

## Ce que ce projet m'a permis de pratiquer

- Les divisions et la structure d'un programme COBOL.
- La déclaration des données avec `PIC` et l'utilisation de tableaux avec `OCCURS`.
- Les conditions, les boucles et l'organisation du programme en paragraphes.
- La validation des entrées, les calculs sur des montants et les tests fonctionnels.

## Auteur

Thomas Dubuc — Étudiant en Techniques de l'informatique au Cégep Édouard-Montpetit.
