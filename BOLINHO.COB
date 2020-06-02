      ************************
      * Author: Bruno Harnik *
      * Purpose: Time Travel *
      ************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. BOLINHO.
           AUTHOR. Bruno Harnik.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ESTOQUE ASSIGN TO DISK
           ORGANIZATION        IS INDEXED
           ACCESS MODE         IS DYNAMIC
           RECORD KEY          IS SKU
           ALTERNATE RECORD KEY    IS NOME WITH DUPLICATES
           FILE STATUS             IS WS-STATUS-ARQUIVO.

           SELECT ARQUIVO-MARGEM ASSIGN TO DISK
           ORGANIZATION        IS INDEXED
           ACCESS MODE         IS DYNAMIC
           RECORD KEY          IS SKU-MARGEM
           ALTERNATE RECORD KEY    IS MARGEM WITH DUPLICATES
           FILE STATUS             IS WS-STATUS-ARQUIVO-MARGEM.
       
       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-ESTOQUE
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "ESTOQUE.DAT".
           01 PRODUTO.
               05 SKU              PIC 9(04).
               05 NOME             PIC X(30).
               05 VALIDADE.
                   10 DIA          PIC 99.
                   10 MES          PIC 99.
                   10 ANO          PIC 9(04).
               05 VALOR-CUSTO      PIC 9(04)V99.
               05 VALOR-VENDA      PIC 9(04)V99.
               05 QTD-ESTOQUE      PIC 9(04).

       FD ARQUIVO-MARGEM
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "MARGEM.DAT".
           01 PRODUTO-MARGEM.
               05 SKU-MARGEM              PIC 9(04).
               05 MARGEM                  PIC 9(04)V99.

       WORKING-STORAGE SECTION.
           77 WS-STATUS-ARQUIVO            PIC X(02).
           77 WS-STATUS-ARQUIVO-MARGEM     PIC X(02).
           77 WS-MENSAGEM                  PIC X(50) VALUE SPACES.
           77 WS-TEMPORIZADOR              PIC 9(05) VALUE ZEROS.

       SCREEN SECTION.
       01 SS-TELA-MENSAGEM.
           05 BLANK SCREEN BACKGROUND-COLOR 04 FOREGROUND-COLOR 15.
           05 PIC X(50) FROM WS-MENSAGEM LINE 13 COLUMN 15.
       
       01 SS-TELA-MENU-PRINCIPAL.
           05 BLANK SCREEN BACKGROUND-COLOR 03 FOREGROUND-COLOR 00.
           05 VALUE "*----------------------------*" LINE 02 COLUMN 25.
           05 VALUE "|  .~~~O~~~.                 |" LINE 03 COLUMN 25.
           05 VALUE "| (         )   Bomboniere   |" LINE 04 COLUMN 25.
           05 VALUE "|  \  ~~~  /    Bolinho      |" LINE 05 COLUMN 25.
           05 VALUE "|   \_____/                  |" LINE 06 COLUMN 25.
           05 VALUE "*----------------------------*" LINE 07 COLUMN 25.
           05 VALUE "Escolha com a letra destacada:" LINE 10 COLUMN 25.
           05 VALUE "C" FOREGROUND-COLOR 15          LINE 12 COLUMN 20.
           05 VALUE "adastrar novo produto"          LINE 12 COLUMN 21.
           05 VALUE "R" FOREGROUND-COLOR 15          LINE 13 COLUMN 20.
           05 VALUE "emover registro de produto"     LINE 13 COLUMN 21.
           05 VALUE "E" FOREGROUND-COLOR 15          LINE 14 COLUMN 20.
           05 VALUE "ditar registro de produto"      LINE 14 COLUMN 21.
           05 VALUE "A" FOREGROUND-COLOR 15          LINE 15 COLUMN 20.
           05 VALUE "valiar estoque"                 LINE 15 COLUMN 21.
           05 VALUE "M" FOREGROUND-COLOR 15          LINE 16 COLUMN 20.
           05 VALUE "ostrar lista completa"          LINE 16 COLUMN 21.
           05 VALUE "S" FOREGROUND-COLOR 15          LINE 17 COLUMN 20.
           05 VALUE "air"                            LINE 17 COLUMN 21.

       PROCEDURE DIVISION.
      *************************
      * Parágrafos Principais *
      *************************
       P-ABERTURA-ARQUIVO-ESTOQUE.
           OPEN I-O ARQUIVO-ESTOQUE
           IF WS-STATUS-ARQUIVO NOT = "00"
               IF WS-STATUS-ARQUIVO = "30"
                   OPEN OUTPUT ARQUIVO-ESTOQUE
                   MOVE "Arquivo de estoque sendo criado..."
                       TO WS-MENSAGEM
                   PERFORM P-MSG-ZERA THRU P-MSG-FIM
                   CLOSE ARQUIVO-ESTOQUE
                   GO TO P-ABERTURA-ARQUIVO-ESTOQUE
               ELSE
                   GO TO P-FIM-EXIT.

       P-ABERTURA-ARQUIVO-MARGEM.
           OPEN I-O ARQUIVO-MARGEM
           IF WS-STATUS-ARQUIVO-MARGEM NOT = "00"
               IF WS-STATUS-ARQUIVO-MARGEM = "30"
                   OPEN OUTPUT ARQUIVO-MARGEM
                   MOVE "Arquivo de margens sendo criado..." 
                       TO WS-MENSAGEM
                   PERFORM P-MSG-ZERA THRU P-MSG-FIM
                   CLOSE ARQUIVO-MARGEM
                   GO TO P-ABERTURA-ARQUIVO-MARGEM
               ELSE
                   GO TO P-FIM-EXIT.
       
       P-MENU-PRINCIPAL.
           

      **************************
      * Parágrafos de mensagem *
      **************************
       P-MSG-ZERA.
           MOVE ZEROS TO WS-TEMPORIZADOR.

       P-MSG-DISPLAY.
           DISPLAY SS-TELA-MENSAGEM.

       P-MSG-TEMPO.
           ADD 1 TO WS-TEMPORIZADOR
           IF WS-TEMPORIZADOR < 2500
               GO TO P-MSG-TEMPO.

       P-MSG-FIM.
           MOVE SPACES TO WS-MENSAGEM
           EXIT.
           
      *****************************
      * Parágrafos de Finalização *
      *****************************
       P-FIM-FECHA-ARQUIVOS.
           CLOSE ARQUIVO-ESTOQUE
           CLOSE ARQUIVO-MARGEM.

       P-FIM-EXIT.
           EXIT PROGRAM.

       P-FIM-STOP-RUN.
           STOP RUN.