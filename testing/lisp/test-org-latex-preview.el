;;; test-org-latex-preview.el --- Cat preview regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-latex-preview)
(require 'ox-html)

(ert-deftest test-org-latex-preview/unsafe-buffer ()
  "Preview dispatch must respect the upstream unsafe-content guard."
  (let ((untrusted-content t)
        (org--latex-preview-when-risky nil)
        dispatched)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
              ((symbol-function 'org-latex-preview--preview-region)
               (lambda (&rest _) (setq dispatched t))))
      (org-latex-preview 'buffer)
      (should-not dispatched)
      (let ((org--latex-preview-when-risky t))
        (org-latex-preview 'buffer)
        (should dispatched)))))

(ert-deftest test-org-latex-preview/dvisvgm-tikz-current-color ()
  "TikZ defaults follow the preview face without losing explicit colors."
  (skip-unless
   (and (executable-find "latex")
        (executable-find "dvisvgm")
        (executable-find "kpsewhich")
        (= 0 (call-process "kpsewhich" nil nil nil "tikz.sty"))))
  (let ((org-latex-preview-cache 'temp)
        (org-latex-preview-process-precompile nil)
        (org-latex-preview-preamble
         (concat "\\documentclass[dvisvgm]{article}\n"
                 "\\usepackage{tikz}\n"
                 "\\usepackage{xcolor}"))
        image)
    (unwind-protect
        (with-temp-buffer
          (let ((org-mode-hook nil)) (org-mode))
          (insert "\\(z\\)\n\\begin{tikzpicture}[domain=0:1.6]
\\draw[->] (-.2,0) -- (2,0) node[below] {\\(x\\)};
\\draw[->] (0,-2) -- (0,2) node[left] {\\(y\\)};
\\draw plot(\\x, {sqrt(\\x^3)});
\\draw plot(\\x, {-sqrt(\\x^3)});
\\draw[red] (0,0) -- (1,1);
\\node [below] at (1,-2) {\\(y^2=ax^3\\)};
\\end{tikzpicture}")
          (apply #'org-async-wait-for
                 (org-latex-preview--place-from-elements
                  'dvisvgm (org-latex-preview-collect-fragments)))
          (let* ((overlay
                  (car (sort
                        (cl-remove-if-not
                         (lambda (ov)
                           (eq (overlay-get ov 'org-overlay-type)
                               'org-latex-overlay))
                         (overlays-in (point-min) (point-max)))
                        (lambda (a b)
                          (> (overlay-start a) (overlay-start b))))))
                 (display (and overlay (overlay-get overlay 'display))))
            (setq image (and display (plist-get (cdr display) :file))))
          (should (file-exists-p image))
          (with-temp-buffer
            (insert-file-contents image)
            (let ((svg (buffer-string)))
              (should (string-match-p "currentColor" svg))
              (should-not
               (string-match-p org-latex-preview--svg-fg-standin svg))
              (should-not
               (string-match-p
                "\\(?:fill\\|stroke\\)=[\"']#000\\(?:000\\)?[\"']" svg))
              (should
               (string-match-p
                "\\(?:#f00\\|#ff0000\\|rgb(255,0,0)\\)" svg)))))
      (when (and image (file-exists-p image))
        (delete-file image)))))

(ert-deftest test-org-latex-preview/xelatex-images-and-cache ()
  (skip-unless (and (executable-find "xelatex") (executable-find "dvisvgm")))
  (let ((org-latex-preview-cache 'temp)
        (org-latex-preview-process-precompile nil)
        (org-latex-compiler "xelatex")
        (org-latex-preview--table nil)
        (org-latex-preview--temp-cache-dir (make-temp-file "preview-cache-" t)))
    (unwind-protect
        (let* ((images (org-latex-preview-create-images
                       '("\\(x^2\\)" "\\[y=2x\\]")
                       :preamble "\\documentclass[dvisvgm]{article}\n\\usepackage{xcolor}"
                       :processing-type 'dvisvgm))
               (cached (org-latex-preview-create-images
                        '("\\(x^2\\)" "\\[y=2x\\]")
                        :preamble "\\documentclass[dvisvgm]{article}\n\\usepackage{xcolor}"
                        :processing-type 'dvisvgm)))
          (should (= (length images) 2))
          (should (equal images cached))
          (dolist (file images)
            (should (file-exists-p file))
            (should (> (file-attribute-size (file-attributes file)) 100))))
      (delete-directory org-latex-preview--temp-cache-dir t))))

(ert-deftest test-org-latex-preview/multiple-tex-commands ()
  (skip-unless (and (executable-find "latex") (executable-find "dvisvgm")))
  (let* ((dir (make-temp-file "preview-chain-" t))
         (marker (expand-file-name "calls" dir))
         (script (expand-file-name "latex-wrapper" dir))
         (org-latex-preview-cache 'temp)
         (org-latex-preview-process-precompile nil)
         (org-latex-compiler "pdflatex")
         (org-latex-preview--table nil)
         (org-latex-preview--temp-cache-dir dir)
         (org-latex-preview-process-alist (copy-tree org-latex-preview-process-alist)))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\necho run >> " (shell-quote-argument marker)
                    "\n" (shell-quote-argument (executable-find "latex"))
                    " \"$@\"\nstatus=$?\n[ \"$status\" -le 1 ]\n"))
          (set-file-modes script #o700)
          (setf (plist-get (alist-get 'dvisvgm org-latex-preview-process-alist) :latex-compiler)
                (list (concat (shell-quote-argument script) " -interaction nonstopmode -output-directory %o %f")
                      (concat (shell-quote-argument script) " -interaction nonstopmode -output-directory %o %f")))
          (let ((image (org-latex-preview-create-images
                        "\\(z=3\\)" :processing-type 'dvisvgm
                        :preamble "\\documentclass{article}\n\\usepackage{xcolor}")))
            (should (file-exists-p image)))
          (with-temp-buffer
            (insert-file-contents marker)
            (should (= (count-lines (point-min) (point-max)) 2))))
      (delete-directory dir t))))

(ert-deftest test-org-latex-preview/html-image-export ()
  (skip-unless (and (executable-find "latex") (executable-find "dvisvgm")))
  (let* ((dir (make-temp-file "preview-html-" t))
         (default-directory dir)
         (org-mode-hook nil)
         (org-latex-preview-cache 'temp)
         (org-latex-preview-process-precompile nil)
         (org-latex-preview--temp-cache-dir dir)
         (org-latex-preview--table nil)
         (org-latex-compiler "pdflatex")
         (org-html-with-latex 'dvisvgm)
         (org-export-with-broken-links nil)
         (org-html-latex-image-options
          '(:foreground "Black" :background "Transparent" :page-width 1.0 :scale 1.0 :image-dir "images")))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#+TITLE: Preview\nA formula: \\(x=1\\).\n")
          (let ((html (org-export-as 'html nil nil t)))
            (should (string-match-p "<img" html))
            (should (string-match-p "images/.*\\.svg" html))
            (should-not (string-match-p "LaTeX preview image not generated" html)))
          (should (directory-files (expand-file-name "images" dir) nil "\\.svg\\'")))
      (delete-directory dir t))))

(ert-deftest test-org-latex-preview/preview-live-editing ()
  (skip-unless (and (executable-find "latex") (executable-find "dvisvgm")))
  (let ((org-mode-hook nil)
        (org-latex-preview-process-precompile nil)
        (org-latex-preview-cache 'temp)
        (org-latex-preview-mode-display-live t)
        (org-latex-preview-process-default 'dvisvgm)
        (org-latex-compiler "pdflatex"))
    (with-temp-buffer
      (org-mode)
      (insert "\\(x=1\\)")
      (goto-char 4)
      (org-latex-preview-mode 1)
      (should (functionp org-latex-preview-live--generator))
      (apply #'org-async-wait-for
             (org-latex-preview--place-from-elements
              'dvisvgm (org-latex-preview-collect-fragments)))
      (let* ((ov (car (overlays-at (point))))
             (old-file (plist-get (cdr (overlay-get ov 'org-preview-image)) :file)))
        (should (file-exists-p old-file))
        (goto-char 5)
        (delete-char 1)
        (insert "2")
        (should (eq (overlay-get ov 'org-preview-state) 'modified))
        (apply #'org-async-wait-for
               (org-latex-preview--place-from-elements
                'dvisvgm (org-latex-preview-collect-fragments)))
        (let ((new-file (plist-get (cdr (overlay-get ov 'org-preview-image)) :file)))
          (should (file-exists-p new-file))
          (should-not (equal old-file new-file))))
      (org-latex-preview-mode -1))))

(ert-deftest test-org-latex-preview/edit-special-live-setup ()
  (let ((org-mode-hook nil)
        (org-latex-preview-process-precompile nil)
        (org-latex-preview-mode-display-live t)
        (org-latex-preview-mode-update-delay 100)
        (org-latex-compiler "pdflatex"))
    (with-temp-buffer
      (org-mode)
      (insert "\\begin{equation}\nx=1\n\\end{equation}\n")
      (goto-char 22)
      (org-latex-preview-mode 1)
      (unwind-protect
          (progn
            (org-edit-special)
            (should (bound-and-true-p org-src-mode))
            (should (functionp org-latex-preview-live--generator))
            (should (eq org-latex-preview-live--element-type 'latex-environment))
            (org-edit-src-exit))
        (when (bound-and-true-p org-src-mode) (org-edit-src-abort))
        (org-latex-preview-mode -1)))))

(provide 'test-org-latex-preview)
;;; test-org-latex-preview.el ends here
