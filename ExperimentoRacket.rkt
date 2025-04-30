#lang racket

(require web-server/servlet
         web-server/servlet-env
         net/url
         xml)

;; --- Definições iniciais ---
(define heuristicas-nielsen
  '("Prevenção de erros e recuperação diante de erros"
    "Consistência e padrões"
    "Correspondência entre o sistema e o mundo real"
    "Visibilidade do status do sistema"
    "Ajuda e documentação"
    "Reconhecimento em vez de memorização"
    "Flexibilidade e eficiência de uso"
    "Controle e liberdade para o usuário" 
    "Estética e design minimalista"))

(define perguntas-drracket
  (vector
   "A plataforma DrRacket possui um sistema de debug para códigos. Quanto isso se encaixa nas heurísticas de prevenção de erros e recuperação diante de erros?"
   "A plataforma DrRacket sempre pede para salvar um novo arquivo criado. Quanto isso se encaixa na heurística de consistência e padrões?"
   "A plataforma DrRacket quando executamos um código sempre mostra no canto inferior direito, a imagem de um homem e verde correndo. Quanto isso se encaixa na heurística de correspondência entre o sistema e o mundo real?"
   "A plataforma DrRacket ao executarmos um código usando servidor web em linguagem Racket, sempre nos mostra que o servidor está em execução/aberto e quando paramos a execução o DrRacket nos mostra que o servidor parou. Quanto isso se encaixa na heurística de visibilidade do status do sistema?"
   "A plataforma DrRacket apresenta um guia de ajuda que nos redireciona para um manual na web, clicando em: Scripts -> Manage -> Help. Quanto isso se encaixa na heurística de ajuda e documentação?"
   "A plataforma DrRacket possui um botão \"play\" verde para clicarmos nele e executarmos o código, semelhante a outros dispositivos eletrônicos. Quanto isso se encaixa na heurística de reconhecimento em vez de memorização?"
   "Na plataforma DrRacket podemos executar os seguintes comandos: Ctrl+C e Crtl+V (copiar), Ctrl+Z (voltar) e Ctrl+S (salvar). Quanto isso se encaixa na heurística de flexibilidade e eficiência de uso?"
   "A plataforma DrRacket deixa executarmos vários tipos diferentes de códigos, desde que eles não apresentem nenhum erro. Quanto isso se encaixa na heurística de controle e liberdade para o usuário?"
   "A plataforma DrRacket possui símbolos e botões com sombreamento, parecendo um 3d. Quanto isso se encaixa na heurística estética e design minimalista?"))

;; --- Função principal ---
(define (start request)
  (define params (request-bindings request))
  
  (define submitted? (assoc 'submit params))
  
  (if submitted?
      (let* ([respostas (for/list ([i (in-range (length heuristicas-nielsen))])
                          (let* ([key (string->symbol (format "q~a" i))]
                                 [binding (assoc key params)])
                            (if binding
                                (string->number (extract-binding/single key params))
                                3)))]
             [comentario (if (assoc 'comentario params)
                            (extract-binding/single 'comentario params)
                            "")])
        
        (render-resultados respostas comentario request))

      ;; Formulário inicial
      (response/xexpr
       `(html
         (head (title "Avaliação de DrRacket"))
         (body
          (h1 "Avaliação de DrRacket segundo Heurísticas de Nielsen")
          (form ((action "") (method "post"))
            ,@(for/list ([idx (in-range (length heuristicas-nielsen))])
                (define pergunta (vector-ref perguntas-drracket idx))
                (define heuristica (list-ref heuristicas-nielsen idx))
                `(fieldset
                  (legend ,heuristica)
                  (p ,pergunta)
                  (div
                   (input ((type "range") 
                           (name ,(format "q~a" idx)) 
                           (min "1") 
                           (max "5") 
                           (value "3")))
                   (span "Valor: 1-5"))))
            (p "Comentários:")
            (textarea ((name "comentario") (rows "4") (cols "40")))
            (br)
            (input ((type "submit") (name "submit") (value "Enviar")))))))))

;; --- Página de resultados ---
(define (render-resultados respostas comentario request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Resultados da Avaliação"))
       (body
        (h1 "Resultados da Avaliação")
        (h2 "Suas respostas:")
        (table ((border "1"))
         (tr (th "Heurística") (th "Pergunta") (th "Valor"))
         ,@(for/list ([idx (in-range (length heuristicas-nielsen))])
             (define heuristica (list-ref heuristicas-nielsen idx))
             (define pergunta (vector-ref perguntas-drracket idx)) 
             (define valor (list-ref respostas idx))
             `(tr
               (td ,heuristica)
               (td ,pergunta)
               (td ,(number->string valor)))))
        (h3 "Comentários:")
        (p ,comentario)
        (p (a ((href ,(embed/url ver-grafico-handler)))
              "Ver Gráfico"))))))
  
  (define (ver-grafico-handler request)
    (render-grafico respostas comentario request))
  
  (send/suspend/dispatch response-generator))

;; --- Página do gráfico ---
(define (render-grafico respostas comentario request)
  ;; Função para gerar uma barra SVG
  (define (criar-barra valor idx)
    (define altura (* valor 30))  ; 30px por ponto na escala 1-5
    (define y-pos (* idx 50))    ; Espaçamento vertical entre barras
    `(g
      (rect ((x "150") 
             (y ,(number->string y-pos))
             (width ,(number->string altura))
             (height "30")
             (fill "steelblue")))
      (text ((x "140") 
             (y ,(number->string (+ y-pos 20)))
             (text-anchor "end")
             (fill "black"))
            ,(format "Q~a:" (+ idx 1)))
      (text ((x ,(number->string (+ altura 160))) 
             (y ,(number->string (+ y-pos 20)))
             (fill "black"))
            ,(number->string valor))))
  
  (define media (/ (apply + respostas) (length respostas)))
  
  (response/xexpr
   `(html
     (head (title "Gráfico de Avaliação"))
     (body
      (h1 "Gráfico de Avaliação")
      (svg ((width "600") (height "500") (xmlns "http://www.w3.org/2000/svg"))
           (g
            ,@(for/list ([valor respostas] [idx (in-range (length respostas))])
                (criar-barra valor idx))
            (line ((x1 "150") (y1 "460") (x2 "450") (y2 "460") (stroke "black")))
            (text ((x "300") (y "480") (text-anchor "middle"))
                  "Pontuação (1-5)")
            (text ((x "300") (y "500") (text-anchor "middle"))
                  ,(format "Média: ~a" (exact->inexact media)))))
      (p (a ((href "/")) "Voltar ao início"))))))

;; --- Iniciar o servidor ---
(serve/servlet start
               #:port 8000
               #:servlet-path "/"
               #:launch-browser? #t)