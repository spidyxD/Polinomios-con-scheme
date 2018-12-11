#lang racket
;PROYECTO 2 PARADIGMAS
;TOMAR EN CUENTA: los polinomios se visualizan asi: '(1 2 3) => 1 + 2x + 3x^2
;==============================================================================
(define eval-p
 (lambda (p x)
   (cond 
      ((= (length p) 0) 0) 
      ((= (length p) 1) (car p))
      (else 
        (+ (car p) (* x (eval-p (cdr p) x)))))))
;(eval-p '(3 1 4 5 0) 2)
;==============================================================================
;SUMA DE POLINOMIOS
(define +p
  (lambda parametros ;Aqui recibe elementos sueltos
    (analizar-sumar parametros)))

;este metodo es para validar lo necesario cuando recibe la lista del metodo anterior
(define analizar-sumar
  (lambda (lista-de-listas)
    (cond
      ((null? lista-de-listas) lista-de-listas)
      ((null? (cdr lista-de-listas)) car lista-de-listas) ;Si solo hay una lista, no hay operaciones que hacer. se necesita minimo 2
      (else
       (sumar lista-de-listas)
       ))))

;si llego aqui es porque de fijo hay mas de una lista. Ej: (cdar '((1 1 1) (2 2 2) (3 3 3) (4 4 4) (5 5 5)))
(define sumar
  (lambda (lista-de-listas)
    (cond
      ((null? lista-de-listas) lista-de-listas)
      ((null? (cdr lista-de-listas)) (car lista-de-listas)) ;No se puede sumar con solo una lista
      (else
       (sumar
        (cons
         (sumar-lista (car lista-de-listas) (cadr lista-de-listas)) ;Le mandamos a sumar-lista el primer y segundo elemento de la lista de listas
         (cddr lista-de-listas)
         ))))))
             
;metodo de sumar
(define sumar-lista
  (lambda (p1 p2)
    (cond
      ((null? p1) p2)
      ((null? p2) p1)
      (else
       (cons (+ (car p1) (car p2))
             (sumar-lista (cdr p1) (cdr p2)))))))
;(+p '(1 2 3) '(2 3 4) '(2 3 5) '(5 -8 2))
;==============================================================================
;RESTA DE POLINOMIOS
(define -p
  (lambda parametros ;Aqui recibe elementos sueltos
    (analizar-restar parametros)))

(define analizar-restar
  (lambda (lista-de-listas)
    (cond
      ((null? lista-de-listas) lista-de-listas)
      ((null? (cdr lista-de-listas)) car lista-de-listas) ;Si solo hay una lista, no hay operaciones que hacer. se necesita minimo 2
      (else
       (restar lista-de-listas)
       ))))

;si llego aqui es porque de fijo hay mas de una lista. Ej: (cdar '((1 1 1) (2 2 2) (3 3 3) (4 4 4) (5 5 5)))
(define restar
  (lambda (lista-de-listas)
    (cond
      ((null? lista-de-listas) lista-de-listas)
      ((null? (cdr lista-de-listas)) (car lista-de-listas)) ;No se puede restar con solo una lista
      (else
       (restar
        (cons
         (restar-lista (car lista-de-listas) (cadr lista-de-listas)) ;Le mandamos a restar-lista el primer y segundo elemento de la lista de listas
         (cddr lista-de-listas)
         ))))))
             
;metodo de restar
(define restar-lista
  (lambda (p1 p2)
    (cond
      ((null? p1) p2)
      ((null? p2) p1)
      (else
       (cons (- (car p1) (car p2))
             (restar-lista (cdr p1) (cdr p2)))))))
;(-p '(1 2 3) '(2 3 4) '(2 3 5) '(5 -8 2))
;==============================================================================
;MULTIPLICACION DE POLINOMIOS
;TOMAR EN CUENTA: los polinomios se ingresan de la forma: '(3 2 1) =>  3x^2 + 2x + 1
(define *p
  (lambda parametros ;Aqui recibe elementos sueltos
    (analizar-multiplicar parametros)))

(define analizar-multiplicar
  (lambda (lista-de-listas)
    (cond
      ((null? lista-de-listas) lista-de-listas)
      ((null? (cdr lista-de-listas)) car lista-de-listas) ;Si solo hay una lista, no hay operaciones que hacer. se necesita minimo 2
      (else
       (multiplicar lista-de-listas)
       ))))

(define multiplicar
  (lambda (lista-de-listas)
    (cond
      ((null? lista-de-listas) lista-de-listas)
      ((null? (cdr lista-de-listas)) (car lista-de-listas)) ;No se puede multiplicar con solo una lista
      (else
       (multiplicar
        (cons
         (multiplicar-lista (car lista-de-listas) (cadr lista-de-listas)) ;Le mandamos a multiplicar-lista el primer y segundo elemento de la lista de listas
         (cddr lista-de-listas)
         ))))))

(define contar
  (lambda (n)
    (if (< n 1)
        '()
        (cons 0 (contar (- n 1))))))
;con este se dara cuenta de que grado es el polinomio
(define grado
  (lambda (p n) ;para mas de un parametro. separar sin comas. Ej: 2 parametros
    (append (contar n) p)))

;Producto escalar de los polinomios
(define escalar
  (lambda (p d)
    (if (null? p)
        '()
        (cons (* d (car p)) (escalar (cdr p) d)))))

;Aqui ya se define la multiplicacion. tambien ocuparemos de la suma para sumar los polinomios de mismo grado 
(define multiplicar-lista
  (lambda (p1 p2)
    (if (null? p2)
        '(0)
        (+p (escalar p1 (car p2))
             (*p (grado p1 1) (cdr p2))))))
;(*p'(1 2)'(2 3 4))
;=============================================================================================================
(define (reversa lista acumulado)
  (if (null? lista)
      acumulado
      (reversa (cdr lista) (cons (car lista) acumulado))))

(define invertir-lista
  (lambda (lista)
  (reversa lista '())))

(define preparar-polinomio
  (lambda (lista)
    (invertir-lista (agregar-exponente lista -1))))

(define agregar-exponente
  (lambda (lista exponente)
    (cond
      ((null? lista) lista)
      ((null? (cdr lista)) (list (list (car lista) (+ 1 exponente))))
      (else
       (cons (list (car lista) (+ 1 exponente)) (agregar-exponente (cdr lista) (+ 1 exponente)))
       ))))


(define (fill-zeroes Lista n)
  (append Lista (make-list (abs n) 0)))

(define (agregar-ceros-izq Lista n)
  (append (make-list (abs n) 0) Lista ))

(define (juntar lst1 lst2) (map (lambda (x y) (list x y)) lst1 lst2))

(define division-1-mal
  (lambda (Dividendo Divisor)
    "No se puede dividir"
    ))

(define borrar-ceros-list
  (lambda (lista)
    (cond
      ((null? lista) lista)
      ((not (= (caar lista) 0)) lista)
      (else
       (borrar-ceros-list (cdr lista)))
      )))

(define borrar-ceros
  (lambda (lista)
    (cond
      ((null? lista) lista)
      ((not (= (car lista) 0)) lista)
      (else
       (borrar-ceros (cdr lista)))
      )))

(define primer-exponente-valido ;primer exponente diferente de 0 
  (lambda (lista)
    (cond
      ((null? lista) 0)
      ((not (= (caar lista) 0)) (cadar lista))
      (else
       (primer-exponente-valido (cdr lista)))
      )))

(define primer-valido ;primer elemento cuyo exponente sea diferente de 0 
  (lambda (lista)
    (cond
      ((null? lista) 0)
      ((not (= (caar lista) 0)) (caar lista))
      (else
       (primer-valido (cdr lista)))
      )))

(define nuevo-dividendo
  (lambda (Dividendo Divisor Elemento)
    (borrar-ceros-list
     (preparar-polinomio
      (invertir-lista
       (map
        -
        (map car Dividendo)
        (map car
            (invertir-lista
             (juntar
              (invertir-lista
               (borrar-ceros
                (invertir-lista
                 (*p
                  (agregar-ceros-izq (list (caar Elemento)) (cadar Elemento))
                  (invertir-lista (map car Divisor))))))
              (map cadr Dividendo))))))))))

(define division-1
  (lambda (Dividendo Divisor Cociente)
    (cond
      ((< (primer-exponente-valido Dividendo) (primer-exponente-valido Divisor)) Cociente)
      (else
       (division-1
        (nuevo-dividendo Dividendo Divisor (list (list (/ (primer-valido Dividendo) (primer-valido Divisor)) (- (primer-exponente-valido Dividendo) (primer-exponente-valido Divisor)))))
        Divisor
        (append
         (list
          (list
           (if (= (primer-valido Divisor) 0) 0 (/ (primer-valido Dividendo) (primer-valido Divisor)))
           (- (primer-exponente-valido Dividendo) (primer-exponente-valido Divisor)))) Cociente)
        )))))

(define division-1-bien
  (lambda (Dividendo Divisor)
    (division-1 Dividendo Divisor '())
    ))

(define residuo
  (lambda (Dividendo Divisor Cociente)
    (cond
      ((< (primer-exponente-valido Dividendo) (primer-exponente-valido Divisor)) Dividendo)
      (else
       (residuo
        (nuevo-dividendo Dividendo Divisor (list (list (/ (primer-valido Dividendo) (primer-valido Divisor)) (- (primer-exponente-valido Dividendo) (primer-exponente-valido Divisor)))))
        Divisor
        (append
         (list
          (list
           (if (= (primer-valido Divisor) 0) 0 (/ (primer-valido Dividendo) (primer-valido Divisor)))
           (- (primer-exponente-valido Dividendo) (primer-exponente-valido Divisor)))) Cociente)
        )))))

(define division-residuo
  (lambda (Dividendo Divisor)
    (residuo Dividendo Divisor '())
    ))

(define alistar
 (lambda (lista num lista-acumulada)
   (cond
     ((null? lista) lista-acumulada)
     ((= (cadar lista) num) (alistar (cdr lista) (+ num 1) (append lista-acumulada (list (car lista)))))
     (else ;Si no coincide es que debemos rellenar con 0...
      (alistar lista (+ num 1) (append lista-acumulada (list (list 0 num))))
      ))))
;==============================================================================
;COCIENTE DE DIVISION DE POLINOMIOS
(define qt-p
 (lambda (Dividendo Divisor)
   (cond
     ((null? Dividendo) Dividendo)
     ((null? Divisor) Divisor)
     ((< (cadar (preparar-polinomio Dividendo)) (cadar (preparar-polinomio Divisor)))
      (division-1-mal Dividendo Divisor))
      (else
       (map car (alistar (division-1-bien (preparar-polinomio Dividendo) (preparar-polinomio (fill-zeroes Divisor (- (length Dividendo) (length Divisor))))) 0 '()))
       ))))
;(qt-p '(0 -15 -8 37 0 -20) '(-5 0 4))
;(qt-p '(1 -1 2 4) '(2 -3 2))
;==============================================================================
;RESIDUO DE DIVISION DE POLINOMIOS
(define rem-p
  (lambda (Dividendo Divisor)
    (cond
     ((null? Dividendo) Dividendo)
     ((null? Divisor) Divisor)
     ((>= (cadar (preparar-polinomio Dividendo)) (cadar (preparar-polinomio Divisor)))
      (map car (alistar (invertir-lista(division-residuo (preparar-polinomio Dividendo) (preparar-polinomio (fill-zeroes Divisor (- (length Dividendo) (length Divisor)))))) 0 '()))
      )
     (else
      (division-residuo Dividendo Divisor)
      ))))
;(rem-p '(0 -15 -8 37 0 -20) '(-5 0 4))
;(rem-p '(1 -1 2 4) '(2 -3 2))
;==============================================================================
;DIVISION DE POLINOMIOS COMPLETA!!!
(define /-p
  (lambda (Dividendo Divisor)
   (cond
     ((< (cadar (preparar-polinomio Dividendo)) (cadar (preparar-polinomio Divisor)))
      (division-1-mal Dividendo Divisor))
      (else
       (cons
        (map car (alistar (division-1-bien (preparar-polinomio Dividendo) (preparar-polinomio (fill-zeroes Divisor (- (length Dividendo) (length Divisor))))) 0 '()))
        (list (map car (alistar (invertir-lista(division-residuo (preparar-polinomio Dividendo) (preparar-polinomio (fill-zeroes Divisor (- (length Dividendo) (length Divisor)))))) 0 '())))
        )))))
;(/-p '(0 -15 -8 37 0 -20) '(-5 0 4))
;(/-p '(1 -1 2 4) '(2 -3 2))
;==============================================================================
;DERIVACION DE POLINOMIOS
(define drv-p
  (lambda parametros ;Aqui recibe elementos sueltos
    (analizar-derivar parametros '())))

(define analizar-derivar
  (lambda (lista-de-listas acumulado)
    (cond
      ((null? lista-de-listas) acumulado)
      ((null? (cdr lista-de-listas)) (derivar-lista (cdar lista-de-listas) 1 '())) ;Para cuando solo mandan una lista...
      (else
       (derivar lista-de-listas acumulado)
       ))))

(define derivar
  (lambda (lista-de-listas acumulado)
    (cond
      ((null? lista-de-listas) acumulado)
      (else
       (derivar
        (cdr lista-de-listas)
        (append acumulado
                (list (derivar-lista (cdar lista-de-listas) 1 '()))
                ))
       ))))

(define derivar-lista
  (lambda (lista numero acumulado)
    (cond
      ((null? lista) acumulado)
      (else
       (derivar-lista
        (cdr lista)
        (+ numero 1)
        (append acumulado (list(* numero (car lista))))
        )))))

;(drv-p '(-2 14 0 9 0 0 7 0 -5))
;(drv-p '(-2 14 0 9 0 0 7 0 -5) '(0 14 5 9 0 2 7 0 9))
;=============================================================================================================
;FACTORIZACION DE POLINOMIOS
;VER VIDEO PARA COMPRENDER LA IDEA PRINCIPAL DE COMO SE RESUEVE
;realiza la busqueda de divisores del coeficiente del polinomio y construye una lista con ellos
(define busca-factor-divisores
  (lambda (L R)
    (cond
      ((null? L) L)
      ((null? R) L)
      ((= (length L)2)(cons (reverse L) '()))
      ((= (car (reverse (f1 (car R) (car L) L 0)))0)(cons (list(* (car R) -1)1)(busca-factor-divisores (reverse(cdr (reverse(f1(car R)(car L)L 0)))) R)))
      ((busca-factor-divisores L (cdr R))))))

;evalua los divisores del coficiente en el polinomio hasta encontrar el divisor  produzca que la evaluacion resulte ser cero
;si es cero se agrega los valores de la evaluacion a una nueva lista que ira construyendo el polinomio factorizado
(define f1
  (lambda (x n L cont)
    (cond
      ((null? L)L)
      ((zero? cont)(cons (car L)(f1 x n (cdr L) (+ cont 1))))
      ((cons (+ (* x n)(car L))(f1 x (+ (* x n) (car L)) (cdr L)(+ cont 1)))))))


;obtiene los divisores de un numero 
(define divisor
  (lambda (x n)
    (cond
      ((< n 0)(divisor x (* n -1)))
      ((= x n) (cons n (list (* n -1))))
      ((= (remainder n x) 0)(cons x (cons (* x -1) (divisor (+ x 1) n))))
      ((divisor (+ x 1) n)))))


;devuelve el ultimo valor de la lista
(define (getUltimo L)
(cond
    ((null? L) #f)
    ((= (- (length L) 1) 0) (car L))
    (else (getUltimo (cdr L)))))

;funcion que se encarga de recibir al polinomio a factorizar
(define fact-p
  (lambda (L)
 (cond
   ((null? L) #f)
  (else (display-p (busca-factor-divisores  (reverse L) (divisor 1 (getUltimo (reverse L)))))))))

(define display-p
   (lambda (L)
      (display-p2 L 0)
      ))

(define display-p2
   (lambda (L exponente)
           (cond ((null? L)
                (display ""))
                  ((zero? exponente)
                   (display (car L)) (display-p2(cdr L)(+ exponente 1)))
                  ((pair?(car L))
                   (display (car L)) (display-p2(cdr L)(+ exponente 1)))
                  ((zero? (car L))(display-p2(cdr L)(+ exponente 1)))
                  ((= exponente 1)(display "+")(display (car L )) (display "x") (display-p2 (cdr L) (+ exponente 1)))
                  (else
                    (display "+")(display (car L))(display "x^")(display exponente)(display-p2 (cdr L)(+ exponente 1))))))

;(display-p '(4 4 1))
;(fact-p '(1 4 4))
;(fact-p '(0 2 0 -2))
