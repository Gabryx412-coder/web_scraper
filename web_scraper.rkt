#lang racket

(require net/http-client)    ; Per le richieste HTTP
(require html-parser)       ; Per il parsing HTML
(require racket/string)      ; Per la manipolazione delle stringhe
(require racket/port)        ; Per la gestione dei flussi di input/output
(require racket/logger)      ; Per il logging
(require racket/async)       ; Per la gestione asincrona
(require racket/match)       ; Per la gestione dei pattern matching
(require racket/date)        ; Per la gestione delle date
(require racket/regex)       ; Per le espressioni regolari

;; Impostiamo il logger per tracciare l'esecuzione
(define logger (make-logger))

;; Funzioni di log
(define (log-info message)
  (logger "INFO" message))

(define (log-error message)
  (logger "ERROR" message))

(define (log-debug message)
  (logger "DEBUG" message))

;; Funzione di configurazione del scraper
(define scraper-config
  '((url "https://example.com")
    (output-file "scraped_data.txt")
    (user-agent "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")))

;; Funzione per estrarre la configurazione
(define (get-config key)
  (cond [(assoc key scraper-config) => cdr]
        [else (error "Configurazione mancante" key)]))

;; Funzione per effettuare richieste HTTP GET con gestione errori
(define (fetch-webpage url)
  "Effettua una richiesta HTTP GET per ottenere il contenuto della pagina."
  (with-handlers ([exn:fail? (lambda (e) (log-error (format "Errore nella richiesta HTTP per ~a" url)) (exit))])
    (begin
      (log-info (format "Inizio richiesta HTTP per ~a" url))
      (define response (http-sendrecv (string->url url) #:headers `(("User-Agent" . ,(get-config 'user-agent)))))
      (define body (response-body response))  ; Otteniamo il corpo della risposta
      (log-info (format "Richiesta riuscita per ~a" url))
      (list->string body))))

;; Funzione di parsing HTML ottimizzata
(define (parse-html-page html)
  "Esegue il parsing HTML e restituisce i dati estratti."
  (with-handlers ([exn:fail? (lambda (e) (log-error "Errore nel parsing HTML") '())]) 
    (begin
      (log-info "Inizio parsing HTML...")
      (define parsed-html (parse html)) 
      (log-info "Parsing completato.")
      parsed-html))

;; Funzione per estrarre i titoli (h1, h2) dalla struttura HTML
(define (extract-titles parsed-html)
  "Estrae titoli h1 e h2 dalla struttura HTML."
  (define (extract-title element)
    (cond
      [(and (pair? element) (member (car element) '(h1 h2))) (cdr element)] 
      [else '()])) ; Altri tag, ignorali
  (define titles (map extract-title (flatten parsed-html))) 
  titles)

;; Funzione per estrarre i link (href) dalla struttura HTML
(define (extract-links parsed-html)
  "Estrae link href dalla struttura HTML."
  (define (extract-link element)
    (cond
      [(and (pair? element) (eq? (car element) 'a)) ; Cerca i tag <a>
       (define href (assoc 'href (cdr element)))   ; Estrai il valore dell'attributo href
       (if href (cdr href) "")]
      [else ""])) 
  (define links (map extract-link (flatten parsed-html))) 
  links)

;; Funzione per visualizzare i risultati estratti
(define (display-results titles links)
  "Visualizza i titoli e i link trovati."
  (log-info "Visualizzazione dei risultati...")
  (displayln "Titoli trovati:")
  (for-each displayln titles)
  (displayln "\nLink trovati:")
  (for-each displayln links))

;; Funzione per salvataggio avanzato dei risultati in un file
(define (save-to-file filename titles links)
  "Salva i titoli e i link estratti in un file."
  (log-info (format "Salvataggio dei risultati su ~a" filename))
  (with-output-to-file filename
    (lambda ()
      (displayln "Titoli:")
      (for-each displayln titles) 
      (displayln "\nLink:")
      (for-each displayln links))))

;; Funzione per ottenere la data attuale in formato leggibile
(define (get-current-date)
  "Restituisce la data e l'ora attuali in formato leggibile."
  (format "~a" (current-date)))

;; Funzione per analizzare la struttura di una pagina HTML con gestione errori avanzata
(define (analyze-html-content url)
  "Analizza la pagina HTML per estrarre titoli e link."
  (log-info (format "Inizio analisi della pagina: ~a" url))
  (define html-content (fetch-webpage url))  ; Ottieni contenuto HTML
  (define parsed-html (parse-html-page html-content)) ; Parsing della pagina
  (if (not parsed-html)
      (begin (log-error "Errore durante il parsing della pagina.") '())
      (begin
        (define titles (extract-titles parsed-html))  ; Estrai titoli
        (define links (extract-links parsed-html))  ; Estrai link
        (display-results titles links) ; Visualizza risultati
        (save-to-file (get-config 'output-file) titles links)  ; Salva i risultati
        (log-info "Analisi completata."))))

;; Funzione per supportare scraping asincrono su più URL
(define (scrape-multiple-pages urls)
  "Effettua scraping asincrono per una lista di URL."
  (define (scrape-url url)
    (log-info (format "Avvio dello scraping asincrono per ~a" url))
    (analyze-html-content url))
  (define futures (map (lambda (url) (async (lambda () (scrape-url url)))) urls))
  (for-each await futures)
  (log-info "Tutti gli scraping asincroni sono completati."))

;; Funzione di scraping completo che gestisce URL dinamici
(define (scrape-page url)
  "Esegui scraping della pagina web e visualizza i dati."
  (let ([html-content (fetch-webpage url)])
    (define parsed-data (parse-html-page html-content))
    (if (not parsed-data)
        (log-error (format "Errore durante il parsing della pagina: ~a" url))
        (begin
          (define titles (extract-titles parsed-data))
          (define links (extract-links parsed-data))
          (display-results titles links)
          (save-to-file (get-config 'output-file) titles links)))))

;; Funzione per ottenere informazioni aggiuntive sulla pagina (metadati)
(define (get-page-metadata url)
  "Estrae metadati dalla pagina HTML come la lingua, titolo, ecc."
  (let ([html-content (fetch-webpage url)])
    (define parsed-data (parse-html-page html-content))
    (define title (car (extract-titles parsed-data)))
    (define links (extract-links parsed-data))
    (log-info (format "Metadata della pagina ~a" url))
    (log-info (format "Titolo della pagina: ~a" title))
    (log-info (format "Numero di link trovati: ~a" (length links)))))

;; Funzione di gestione avanzata della configurazione
(define (configure-scraper)
  "Carica e applica la configurazione dello scraper."
  (define config-url (get-config 'url))
  (define config-output (get-config 'output-file))
  (define config-agent (get-config 'user-agent))
  (log-info (format "Configurazione applicata. URL: ~a, File di output: ~a, User-Agent: ~a" config-url config-output config-agent)))

;; Funzione di test avanzato per lo scraper
(define (test-scraper)
  "Funzione di test per il web scraper."
  (log-info "Esecuzione del test del web scraper...")
  (scrape-page "https://example.com"))

;; Funzione per avviare e testare il web scraper
(define (run-scraper)
  "Funzione di avvio per il web scraper."
  (configure-scraper)
  (test-scraper)
  (scrape-multiple-pages '("https://example.com" "https://example.org" "https://example.net")))

(run-scraper)  ; Avvia lo scraper con l'URL configurato
