CCONVERT.lsp - V0.1

;;; Lisp-programma voor het transformeren van Lambert 72 naar Lambert 2008 en vice versa.
;;; Roept de cconvert API van het NGI aan om entiteiten samen te verplaatsen, volgens het midden van de bounding box.
;;; Entiteiten zullen afzonderlijk en inclusief hoogtes getransformeerd worden vanaf V1.1, om bijgevolg ook voor grotere zones nauwkeurig toegepast te kunnen worden.
;;; Voorlopig dus enkel nauwkeurig bruikbaar voor kleine zones (perceelsniveau)!
;;; Nog enkel getest op BricsCAD V26, werkt vermoedelijk ook op eerdere versies en AutoCAD.
;;; De eindgebruiker blijft zelf verantwoordelijk!

(vl-load-com)

(setq *cc-url*
  "https://cconvert.geo.be/coordinateTransformations/v2/convertCoordinates")

;;; --- Build the JSON request body for a single point -------------------------
;;; iProj/oProj are always LAMBERT; the DATUM picks which Belgian Lambert:
;;;   LAMBERT + BD72   -> Lambert 72   (EPSG:31370)
;;;   LAMBERT + ETRS89 -> Lambert 2008 (EPSG:3812)
(defun cc:build-body (i-datum o-datum x y)
  (strcat
    "{"
    "\"iDatum\":\""     i-datum "\","
    "\"iProj\":\"LAMBERT\","
    "\"iCoordType\":\"PLANE\","
    "\"iUnitType\":\"DEFAULT\","
    "\"oDatum\":\""     o-datum "\","
    "\"oProj\":\"LAMBERT\","
    "\"oCoordType\":\"PLANE\","
    "\"oUnitType\":\"DEFAULT\","
    "\"iCoords\":[[\"" (rtos x 2 4) "\",\"" (rtos y 2 4) "\",\"0\"]]"
    "}"))

;;; --- POST the body, return (status responseText) or nil ---------------------
(defun cc:http-post (url body / http err status resp)
  (setq http (vl-catch-all-apply 'vlax-create-object
                                 (list "WinHttp.WinHttpRequest.5.1")))
  (if (vl-catch-all-error-p http)
    (progn
      (princ "\nWinHTTP is not available (this works on Windows only).")
      nil)
    (progn
      (setq err (vl-catch-all-apply
                  '(lambda ()
                     (vlax-invoke-method http 'Open "POST" url :vlax-false)
                     (vlax-invoke-method http 'SetRequestHeader
                                         "Content-Type" "application/json")
                     (vlax-invoke-method http 'SetRequestHeader
                                         "Accept" "application/json")
                     (vlax-invoke-method http 'Send body))))
      (if (vl-catch-all-error-p err)
        (progn
          (princ (strcat "\nRequest failed: " (vl-catch-all-error-message err)))
          (vl-catch-all-apply 'vlax-release-object (list http))
          nil)
        (progn
          (setq status (vlax-get-property http 'Status)
                resp   (vlax-get-property http 'ResponseText))
          (vlax-release-object http)
          (list status resp))))))

;;; --- Pull the first  "values":[ ... ]  array text out of the response -------
(defun cc:first-values (s / p a b)
  (if (setq p (vl-string-search "\"values\"" s))
    (if (setq a (vl-string-search "[" s p))
      (if (setq b (vl-string-search "]" s a))
        (substr s (+ a 2) (- b a 1))
        "")
      "")
    ""))

;;; --- Turn "x,y,z" into a list of reals (quotes/brackets/spaces ignored) -----
(defun cc:nums (s / lst tok i n ch)
  (setq lst '() tok "" i 1 n (strlen s))
  (while (<= i n)
    (setq ch (substr s i 1))
    (cond
      ((= ch ",")
       (if (> (strlen tok) 0) (setq lst (cons (atof tok) lst)))
       (setq tok ""))
      ((member ch '("\"" " " "\t" "\n" "\r" "[" "]")) nil)
      (t (setq tok (strcat tok ch))))
    (setq i (1+ i)))
  (if (> (strlen tok) 0) (setq lst (cons (atof tok) lst)))
  (reverse lst))

;;; --- Bounding-box centre of a selection set, as (x y) -----------------------
(defun cc:center (ss / i ent obj mn mx p1 p2 minx miny maxx maxy)
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent))
    (if (not (vl-catch-all-error-p
               (vl-catch-all-apply 'vla-getboundingbox (list obj 'mn 'mx))))
      (progn
        (setq p1 (vlax-safearray->list mn)
              p2 (vlax-safearray->list mx))
        (if (null minx)
          (setq minx (car p1) miny (cadr p1) maxx (car p2) maxy (cadr p2))
          (setq minx (min minx (car p1)) miny (min miny (cadr p1))
                maxx (max maxx (car p2)) maxy (max maxy (cadr p2))))))
    (setq i (1+ i)))
  (if minx (list (/ (+ minx maxx) 2.0) (/ (+ miny maxy) 2.0))))

;;; --- The command ------------------------------------------------------------
(defun C:CCONVERT (/ dir sel i-datum o-datum label ss center res
                     vals nx ny dx dy oce)
  (initget "72to2008 2008to72")
  (setq dir (cond ((getkword "\nDirection [72to2008/2008to72] <72to2008>: "))
                  ("72to2008")))
  (if (= dir "72to2008")
    (setq i-datum "BD72"   o-datum "ETRS89" label "Lambert 72 -> Lambert 2008")
    (setq i-datum "ETRS89" o-datum "BD72"   label "Lambert 2008 -> Lambert 72"))

  (initget "All Select")
  (setq sel (cond ((getkword "\nEntities [All/Select] <Select>: ")) ("Select")))
  (princ "\nSelect entities...")
  ;; "All" is limited to the current space (model or the active layout).
  (setq ss (if (= sel "All")
             (ssget "_X" (list (cons 410 (getvar "ctab"))))
             (ssget)))

  (cond
    ((null ss) (princ "\nNothing selected."))

    ((null (setq center (cc:center ss)))
     (princ "\nCould not determine a bounding box for the selection."))

    (t
     (princ (strcat "\n" label " - transforming centre "
                    (rtos (car center) 2 3) ", " (rtos (cadr center) 2 3) " ..."))
     (setq res (cc:http-post *cc-url*
                 (cc:build-body i-datum o-datum (car center) (cadr center))))
     (cond
       ((null res)
        (princ "\nNo response from the API. Nothing moved."))

       ((/= (car res) 200)
        (princ (strcat "\nAPI returned HTTP " (itoa (car res))
                       ". Nothing moved.\n" (cadr res))))

       (t
        (setq vals (cc:nums (cc:first-values (cadr res))))
        (if (< (length vals) 2)
          (progn
            (princ "\nCould not read coordinates from the response. Nothing moved.")
            (princ (strcat "\nResponse was:\n" (cadr res))))
          (progn
            (setq nx (car vals) ny (cadr vals)
                  dx (- nx (car center)) dy (- ny (cadr center))
                  oce (getvar "cmdecho"))
            (setvar "cmdecho" 0)
            (command "_.move" ss "" "_non" (list 0.0 0.0 0.0)
                                       "_non" (list dx dy 0.0))
            (setvar "cmdecho" oce)
            (princ (strcat "\nDone. Moved " (itoa (sslength ss))
                           " entities by dx=" (rtos dx 2 4)
                           " dy=" (rtos dy 2 4) "."))))))))
  (princ))

(princ "\nCCONVERT loaded.  Type CCONVERT to run.")
(princ)
