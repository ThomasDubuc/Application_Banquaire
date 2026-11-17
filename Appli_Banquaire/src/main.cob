       IDENTIFICATION DIVISION. 
       PROGRAM-ID. BankingSystem.
       AUTHOR. Thomas Dubuc.


       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-COMPTES
               ASSIGN TO "comptes.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD FICHIER-COMPTES.
       01 WS-COMPTES-ECRIT.
           05 WS-COMPTE-ECRIT 
               10 WS-NUM-ECRIT PIC 9(6).
               10 WS-NOM-ECRIT PIC X(30).
               10 WS-SOLDE-ECRIT PIC 9(7)V99.

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
       01 WS-MONTANT                   PIC 9(7)V99.
       01 WS-MONTANT-SAISI             PIC X(20).
       01 WS-MONTANT-AFFICHAGE         PIC Z(6)9.99.
       01 WS-MONTANT-VALIDE-OK                  PIC X(1) VALUE "N".
           88 WS-MONTANT-VALIDE-OK-OUI          VALUE "Y".
           88 WS-MONTANT-VALIDE-OK-NON          VALUE "N".
       01 WS-ANCIEN-SOLDE              PIC 9(7)V99.
       01 WS-ANCIEN-SOLDE-AFFICHAGE    PIC Z(6)9.99.
       01 WS-INDEX-COMPTE-TROUVE       PIC 9(1).
       01 WS-INDEX-SOURCE      PIC 9(1).
       01 WS-INDEX-DESTINATION PIC 9(1).
       01 WS-POSITION-DECIMALE         PIC 9(2) VALUE 0.
       01 WS-NB-DECIMALES              PIC 9(2) VALUE 0.
       01 WS-LONGUEUR-MONTANT          PIC 9(2) VALUE 0.
       01 WS-I                         PIC 9(2) VALUE 0.


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
               DISPLAY "4. Effectuer un depot"
               DISPLAY "5. Effectuer un retrait"
               DISPLAY "6. Effectuer un virement"
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
                   WHEN "4"
                       PERFORM DEPOT-COMPTE
                   WHEN "5"
                       PERFORM RETRAIT-COMPTE
                   WHEN "6"
                       PERFORM VIREMENT-COMPTE
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
                           PERFORM TROUVER-COMPTE
                           IF WS-COMPTE-TROUVER-OUI                 
                               MOVE WS-SOLDE(WS-INDEX)
                               TO WS-SOLDE-AFFICHAGE
                               DISPLAY " "
                               DISPLAY "Compte trouve !"
                               DISPLAY " "
                               DISPLAY "Numero de compte : " 
                               WS-NUM (WS-INDEX)
                               DISPLAY "Titulaire        : " 
                               FUNCTION TRIM(WS-NOM (WS-INDEX))
                               DISPLAY "Solde actuel    : " 
                               FUNCTION TRIM(WS-SOLDE-AFFICHAGE) 
                               "$"
                           END-IF    
                           IF WS-COMPTE-TROUVER-NON
                               DISPLAY " "
                               DISPLAY "Aucun compte trouve."
                           END-IF
                   END-EVALUATE
           END-PERFORM.


       DEPOT-COMPTE.
           MOVE SPACES TO WS-RECHERCHE-NUM-SAISI
           PERFORM UNTIL FUNCTION LENGTH(
              FUNCTION TRIM(WS-RECHERCHE-NUM-SAISI TRAILING)
              ) = 6
              AND WS-RECHERCHE-NUM-SAISI(1:6) IS NUMERIC
              AND WS-COMPTE-TROUVER-OUI
                DISPLAY " "
                DISPLAY "===================================="
                DISPLAY "DEPOT BANCAIRE"
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
                       PERFORM TROUVER-COMPTE
                       IF WS-COMPTE-TROUVER-NON
                           DISPLAY " "
                           DISPLAY "Aucun compte trouve."
                           EXIT PERFORM
                       END-IF
                       MOVE WS-SOLDE(WS-INDEX-COMPTE-TROUVE)
                       TO WS-ANCIEN-SOLDE
                       PERFORM SAISIR-MONTANT
                       IF WS-MONTANT-VALIDE-OK-OUI
                          IF WS-MONTANT + 
                          WS-SOLDE(WS-INDEX-COMPTE-TROUVE) > 9999999.99
                             DISPLAY " "
                             DISPLAY 
                             "Le solde ne doit pas depasser 9999999.99"
                             EXIT PERFORM
                          END-IF
                          ADD WS-MONTANT 
                          TO WS-SOLDE(WS-INDEX-COMPTE-TROUVE)
                          MOVE WS-SOLDE(WS-INDEX-COMPTE-TROUVE)
                          TO WS-SOLDE-AFFICHAGE
                          MOVE WS-ANCIEN-SOLDE 
                          TO WS-ANCIEN-SOLDE-AFFICHAGE
                          MOVE WS-MONTANT TO WS-MONTANT-AFFICHAGE
                          DISPLAY " "
                          DISPLAY "Depot effectue avec succes !"
                          DISPLAY " "
                          DISPLAY "Ancien solde : " 
                          FUNCTION TRIM(WS-ANCIEN-SOLDE-AFFICHAGE)"$"
                          DISPLAY "Montant depose : " 
                          FUNCTION TRIM(WS-MONTANT-AFFICHAGE) "$"
                          DISPLAY "Nouveau solde : " 
                          FUNCTION TRIM(WS-SOLDE-AFFICHAGE) "$"
                       END-IF  
                END-EVALUATE
           END-PERFORM.


       RETRAIT-COMPTE.
           MOVE SPACES TO WS-RECHERCHE-NUM-SAISI
           PERFORM UNTIL FUNCTION LENGTH(
              FUNCTION TRIM(WS-RECHERCHE-NUM-SAISI TRAILING)
              ) = 6
              AND WS-RECHERCHE-NUM-SAISI(1:6) IS NUMERIC
              AND WS-COMPTE-TROUVER-OUI
                DISPLAY " "
                DISPLAY "===================================="
                DISPLAY "RETRAIT BANCAIRE"
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
                       PERFORM TROUVER-COMPTE
                       IF WS-COMPTE-TROUVER-NON
                           DISPLAY " "
                           DISPLAY "Aucun compte trouve."
                           EXIT PERFORM
                       END-IF
                       MOVE WS-SOLDE(WS-INDEX-COMPTE-TROUVE)
                       TO WS-ANCIEN-SOLDE
                       PERFORM SAISIR-MONTANT
                       IF WS-MONTANT-VALIDE-OK-OUI
                          IF WS-SOLDE(WS-INDEX-COMPTE-TROUVE) - 
                           WS-MONTANT < 0
                             DISPLAY " "
                             DISPLAY 
                             "Le solde ne doit pas etre negatif"
                             EXIT PERFORM
                          END-IF
                          SUBTRACT WS-MONTANT 
                          FROM WS-SOLDE(WS-INDEX-COMPTE-TROUVE)
                          MOVE WS-SOLDE(WS-INDEX-COMPTE-TROUVE)
                          TO WS-SOLDE-AFFICHAGE
                          MOVE WS-ANCIEN-SOLDE 
                          TO WS-ANCIEN-SOLDE-AFFICHAGE
                          MOVE WS-MONTANT TO WS-MONTANT-AFFICHAGE
                          DISPLAY " "
                          DISPLAY "Depot effectue avec succes !"
                          DISPLAY " "
                          DISPLAY "Ancien solde : " 
                          FUNCTION TRIM(WS-ANCIEN-SOLDE-AFFICHAGE)"$"
                          DISPLAY "Montant depose : " 
                          FUNCTION TRIM(WS-MONTANT-AFFICHAGE) "$"
                          DISPLAY "Nouveau solde : " 
                          FUNCTION TRIM(WS-SOLDE-AFFICHAGE) "$"
                       END-IF  
                END-EVALUATE
           END-PERFORM.
           
       VIREMENT-COMPTE.
           MOVE SPACES TO WS-RECHERCHE-NUM-SAISI
           MOVE 0 TO WS-INDEX-SOURCE
           MOVE 0 TO WS-INDEX-DESTINATION
              DISPLAY " "
              DISPLAY "===================================="
              DISPLAY "VIREMENT ENTRE COMPTES"
              DISPLAY "===================================="
              DISPLAY " "
              PERFORM UNTIL FUNCTION LENGTH(
                 FUNCTION TRIM(WS-RECHERCHE-NUM-SAISI TRAILING)
                 ) = 6
                 AND WS-RECHERCHE-NUM-SAISI(1:6) IS NUMERIC
                 DISPLAY "Numero du compte source : " WITH NO ADVANCING
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
                       PERFORM TROUVER-COMPTE
                       IF WS-COMPTE-TROUVER-NON
                           DISPLAY " "
                           DISPLAY "Aucun compte trouve."
                           EXIT PERFORM
                       END-IF
                       MOVE WS-INDEX-COMPTE-TROUVE TO WS-INDEX-SOURCE
                 END-EVALUATE
              END-PERFORM   
              MOVE SPACES TO WS-RECHERCHE-NUM-SAISI
              IF WS-COMPTE-TROUVER-OUI
              PERFORM UNTIL WS-INDEX-DESTINATION > 0
                 DISPLAY " "
                 DISPLAY "Numero du compte destination : "
                 WITH NO ADVANCING
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
                       PERFORM TROUVER-COMPTE
                       EVALUATE TRUE
                          WHEN WS-COMPTE-TROUVER-NON
                           DISPLAY " "
                           DISPLAY "Aucun compte trouve."
                           EXIT PERFORM
                          WHEN  WS-INDEX-SOURCE = WS-INDEX-COMPTE-TROUVE
                           DISPLAY " "
                           DISPLAY "Erreur : comptes identiques."
                          WHEN  OTHER
                           MOVE WS-INDEX-COMPTE-TROUVE 
                           TO WS-INDEX-DESTINATION
                       END-EVALUATE
                 END-EVALUATE
              END-PERFORM
              IF WS-INDEX-SOURCE > 0 AND WS-INDEX-DESTINATION > 0     
              DISPLAY " "
              DISPLAY "===================================="
              DISPLAY "COMPTE SOURCE"
              DISPLAY "===================================="
              DISPLAY " "      
              DISPLAY "Titulaire : "
              FUNCTION TRIM(WS-NOM (WS-INDEX-SOURCE))
              MOVE WS-SOLDE(WS-INDEX-SOURCE) TO WS-SOLDE-AFFICHAGE
              DISPLAY "Solde actuel : " 
              FUNCTION TRIM(WS-SOLDE-AFFICHAGE) "$"
              DISPLAY " "
              DISPLAY "===================================="
              DISPLAY "COMPTE DESTINATION"
              DISPLAY "===================================="
              DISPLAY " "        
              DISPLAY "Titulaire : " FUNCTION 
              TRIM(WS-NOM (WS-INDEX-DESTINATION))
              MOVE WS-SOLDE(WS-INDEX-DESTINATION) TO WS-SOLDE-AFFICHAGE
              DISPLAY "Solde actuel : " 
              FUNCTION TRIM(WS-SOLDE-AFFICHAGE) "$"
              PERFORM SAISIR-MONTANT
                IF WS-MONTANT-VALIDE-OK-OUI
                EVALUATE TRUE
                     WHEN WS-MONTANT > WS-SOLDE(WS-INDEX-SOURCE)
                        DISPLAY " "
                        DISPLAY "Erreur : solde insuffisant."
                     WHEN  WS-MONTANT + WS-SOLDE(WS-INDEX-DESTINATION) 
                     > 9999999.99
                        DISPLAY " "
                        DISPLAY "Erreur : limite depassee.."
                     WHEN  OTHER
                        SUBTRACT WS-MONTANT 
                        FROM WS-SOLDE(WS-INDEX-SOURCE)
                        ADD WS-MONTANT TO WS-SOLDE(WS-INDEX-DESTINATION)
                        MOVE WS-SOLDE(WS-INDEX-SOURCE) 
                        TO WS-SOLDE-AFFICHAGE
                        DISPLAY " "
                        DISPLAY "Virement effectue avec succes !"
                        DISPLAY " "
                        DISPLAY "Nouveau solde du compte source : " 
                        FUNCTION TRIM(WS-SOLDE-AFFICHAGE) "$"
                        MOVE WS-SOLDE(WS-INDEX-DESTINATION) 
                        TO WS-SOLDE-AFFICHAGE
                        DISPLAY "Nouveau solde du compte destination : " 
                        FUNCTION TRIM(WS-SOLDE-AFFICHAGE) "$"
                END-EVALUATE
                END-IF
              END-IF
              END-IF
           DISPLAY " ".

       TROUVER-COMPTE.
           SET WS-COMPTE-TROUVER-NON TO TRUE
           MOVE FUNCTION NUMVAL(WS-RECHERCHE-NUM-SAISI)
           TO WS-RECHERCHE-NUM
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-NB-COMPTES
                   IF WS-NUM (WS-INDEX) = WS-RECHERCHE-NUM
                       SET WS-COMPTE-TROUVER-OUI TO TRUE
                       MOVE WS-INDEX TO WS-INDEX-COMPTE-TROUVE
                       EXIT PERFORM 
                   END-IF
           END-PERFORM.  


       SAISIR-MONTANT.
           MOVE SPACES TO WS-MONTANT-SAISI
           SET WS-MONTANT-VALIDE-OK-NON TO TRUE
           PERFORM UNTIL WS-MONTANT-VALIDE-OK-OUI
                          DISPLAY " "
                          DISPLAY "Montant de la transaction : " 
                          WITH NO ADVANCING
                          ACCEPT WS-MONTANT-SAISI
                       
                       MOVE FUNCTION LENGTH(
                          FUNCTION TRIM(WS-MONTANT-SAISI)
                       ) TO WS-LONGUEUR-MONTANT
                       MOVE 0 TO WS-POSITION-DECIMALE
                       MOVE 0 TO WS-NB-DECIMALES
                       PERFORM VARYING WS-I FROM 1 BY 1
                          UNTIL WS-I > WS-LONGUEUR-MONTANT
                             IF FUNCTION TRIM(WS-MONTANT-SAISI)(WS-I:1)
                             = "."
                                MOVE WS-I TO WS-POSITION-DECIMALE
                             END-IF
                       END-PERFORM
                       IF WS-POSITION-DECIMALE > 0
                          COMPUTE WS-NB-DECIMALES =
                          WS-LONGUEUR-MONTANT - WS-POSITION-DECIMALE
                       END-IF
                       EVALUATE TRUE
                          WHEN FUNCTION TEST-NUMVAL(
                          FUNCTION TRIM(WS-MONTANT-SAISI TRAILING)
                          ) NOT = 0
                             DISPLAY " "
                             DISPLAY 
                             "Veuillez entrer un montant numerique"
                          WHEN FUNCTION NUMVAL(WS-MONTANT-SAISI) <= 0
                             DISPLAY " "
                             DISPLAY 
                             "Le montant doit etre superieur a 0"

                          WHEN FUNCTION NUMVAL(WS-MONTANT-SAISI)
                           > 100000.00
                             DISPLAY " "
                             DISPLAY 
                             "Le montant ne doit pas depasser 100 000"
                          WHEN WS-NB-DECIMALES > 2
                             DISPLAY " "
                             DISPLAY 
                             "Maximum 2 chiffres apres la virgule"
                          WHEN OTHER
                             SET WS-MONTANT-VALIDE-OK-OUI TO TRUE
                             MOVE FUNCTION NUMVAL(WS-MONTANT-SAISI)
                             TO WS-MONTANT
                       END-EVALUATE
                      END-PERFORM.

      

       END PROGRAM BankingSystem.
       