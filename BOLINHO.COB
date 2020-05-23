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
           SELECT ARQUIVO ASSIGN TO DISK
           ORGANIZATION        IS INDEXED
           ACCESS MODE         IS DYNAMIC
           RECORD KEY          IS SKU
           ALTERNATE RECORD KEY    IS NOME WITH DUPLICATES
           FILE STATUS             IS WS-STATUS-ARQUIVO.
       
       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO
           LABEL RECORDS ARE STANDARD
           VALUE OF FILE-ID IS "BOLINHO.DAT".
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

       WORKING-STORAGE SECTION.
           77 WS-STATUS-ARQUIVO    PIC X(02).

       SCREEN SECTION.

       PROCEDURE DIVISION.
       P-INTRO.
           DISPLAY "Hello World!".

       P-FIM-EXIT.
           EXIT PROGRAM.

       P-FIM-STOP-RUN.
           STOP RUN.