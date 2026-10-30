       IDENTIFICATION DIVISION. 
       PROGRAM-ID. BankingSystem.
       AUTHOR. Thomas Dubuc.

       DATA DIVISION.

       WORKING-STORAGE SECTION. 
       
       01 WS-MENU-CHOICE               PIC X(1) VALUE SPACE.
       01 WS-NB-COMPTES                PIC 9(1) VALUE 0. 
       01 WS-INDEX                     PIC 9(1) VALUE 1.
       01 WS-COMPTES.
           05 WS-COMPTE OCCURS 5 TIMES.
               10 WS-NUM               PIC 9(6).
               10 WS-NOM               PIC X(30).
               10 WS-SOLDE             PIC 9(7)V99.
       01 WS-NOM-SAISI                 PIC X(30).
       01 WS-SOLDE-AFFICHAGE           PIC Z(6)9.99.
       01 WS-RECHERCHE-NUM             PIC 9(6).
       01 WS-RECHERCHE-NUM-SAISI       PIC X(20).
       01 WS-COMPTE-TROUVER            PIC X(1) VALUE "N".
           88 WS-COMPTE-TROUVER-OUI  VALUE "Y".
           88 WS-COMPTE-TROUVER-NON  VALUE "N".

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           PERFORM UNTIL WS-MENU-CHOICE = "0"
               DISPLAY " "
               DISPLAY "===================================="
               DISPLAY "COBOL BANKING SYSTEM"
               DISPLAY "===================================="
               DISPLAY " "
               DISPLAY "1. Creer un compte"
               DISPLAY "2. Afficher les comptes"
               DISPLAY "3. Rechercher un compte"
               DISPLAY "0. Quitter"
               DISPLAY " "
               DISPLAY "Votre choix : " WITH NO ADVANCING
               ACCEPT WS-MENU-CHOICE
               EVALUATE WS-MENU-CHOICE
                   WHEN "1"
                       PERFORM CREER-COMPTE
                   WHEN "2"
                       PERFORM LISTE-COMPTE
                   WHEN "3"
                       PERFORM RECHERCHE-COMPTE
                   WHEN "0"
                       DISPLAY " "
                       DISPLAY "Fermeture du programme..."
                   WHEN OTHER
                       DISPLAY " "
                       DISPLAY "Option invalide, veuillez reessayer."
               END-EVALUATE
               DISPLAY " "
           END-PERFORM.
           STOP RUN.


       CREER-COMPTE.
           DISPLAY " "
           DISPLAY "====================================".
           DISPLAY "CREATION D'UN COMPTE".
           DISPLAY "====================================".
           DISPLAY " "

           IF WS-NB-COMPTES < 5
               DISPLAY "Nom du titulaire : " WITH NO ADVANCING
               ACCEPT WS-NOM-SAISI
               ADD 1 TO WS-NB-COMPTES
               COMPUTE WS-NUM (WS-NB-COMPTES) = 100000 + WS-NB-COMPTES
               MOVE 0.00 TO WS-SOLDE (WS-NB-COMPTES)
               MOVE WS-NOM-SAISI TO WS-NOM (WS-NB-COMPTES)
               MOVE WS-SOLDE (WS-NB-COMPTES) TO WS-SOLDE-AFFICHAGE
               DISPLAY " "
               DISPLAY "Compte creer avec succes!"
               DISPLAY " "
               DISPLAY "Numero de compte : " WS-NUM (WS-NB-COMPTES)
               DISPLAY "Titulaire        : " 
               FUNCTION TRIM(WS-NOM (WS-NB-COMPTES))
               DISPLAY "Solde initial    : " 
               FUNCTION TRIM(WS-SOLDE-AFFICHAGE) 
               "$"
           ELSE
               DISPLAY "ERREUR : Nombre maximal de comptes atteint"
           END-IF.
               

       LISTE-COMPTE.
           DISPLAY " "
           DISPLAY "====================================".
           DISPLAY "LISTE DES COMPTES".
           DISPLAY "====================================".
           DISPLAY " "
           IF WS-NB-COMPTES = 0
               DISPLAY "Aucun compte bancaire disponible"
           ELSE
               PERFORM VARYING WS-INDEX FROM 1 BY 1
                   UNTIL WS-INDEX > WS-NB-COMPTES
                   
                   DISPLAY WS-NUM (WS-INDEX)
                   DISPLAY FUNCTION TRIM(WS-NOM (WS-INDEX))
                   DISPLAY " "

               END-PERFORM
               DISPLAY " "
               DISPLAY "===================================="
               DISPLAY "Nombre de comptes : " WS-NB-COMPTES
               DISPLAY "===================================="
               DISPLAY " "
           END-IF.

       RECHERCHE-COMPTE.
           MOVE SPACES TO WS-RECHERCHE-NUM-SAISI
           PERFORM UNTIL FUNCTION LENGTH(
               FUNCTION TRIM(WS-RECHERCHE-NUM-SAISI TRAILING)
               ) = 6
               AND WS-RECHERCHE-NUM-SAISI(1:6) IS NUMERIC
                   SET WS-COMPTE-TROUVER-NON TO TRUE
                   DISPLAY " "
                   DISPLAY "===================================="
                   DISPLAY "RECHERCHE D'UN COMPTE"
                   DISPLAY "===================================="
                   DISPLAY " "
                   DISPLAY "Numero de compte : " WITH NO ADVANCING
                   ACCEPT WS-RECHERCHE-NUM-SAISI
                   EVALUATE TRUE
                       WHEN FUNCTION LENGTH(
                       FUNCTION TRIM(WS-RECHERCHE-NUM-SAISI TRAILING)
                       ) NOT EQUAL TO 6
                           DISPLAY " "
                           DISPLAY "Longueur invalide !"
                       WHEN WS-RECHERCHE-NUM-SAISI(1:6) IS NOT NUMERIC
                           DISPLAY " "
                           DISPLAY "Le numero doit contenir 6 chiffres."
                       WHEN OTHER
                           MOVE FUNCTION NUMVAL(WS-RECHERCHE-NUM-SAISI)
                           TO WS-RECHERCHE-NUM
                           PERFORM VARYING WS-INDEX FROM 1 BY 1
                               UNTIL WS-INDEX > WS-NB-COMPTES
                                   IF WS-NUM (WS-INDEX) 
                                   = WS-RECHERCHE-NUM
                                       SET WS-COMPTE-TROUVER-OUI TO TRUE
                                       MOVE WS-SOLDE(WS-INDEX)
                                       TO WS-SOLDE-AFFICHAGE
                                       DISPLAY " "
                                       DISPLAY "Compte trouve !"
                                       DISPLAY " "
                                       DISPLAY "Numero de compte : " 
                                       WS-NUM (WS-INDEX)
                                       DISPLAY "Titulaire        : " 
                                       FUNCTION TRIM(WS-NOM (WS-INDEX))
                                       DISPLAY "Solde initial    : " 
                                       FUNCTION TRIM(WS-SOLDE-AFFICHAGE) 
                                       "$"
                                       EXIT PERFORM 
                                   END-IF         
                           END-PERFORM
                           IF WS-COMPTE-TROUVER-NON
                               DISPLAY " "
                               DISPLAY "Aucun compte trouve."
                           END-IF
                   END-EVALUATE
           END-PERFORM.
           
       END PROGRAM BankingSystem.
       