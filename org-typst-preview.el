;;; org-typst-preview.el --- Preview typst in org mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 remimimimimi
;;
;; Author: remimimimimi <remimimimimi@protonmail.com>
;; Maintainer: remimimimimi <remimimimimi@protonmail.com>
;; Created: May 18, 2024
;; Modified: May 21, 2024
;; Version: 1.0.0
;; Keywords: abbrev convenience docs faces tex text
;; Homepage: https://github.com/remimimimimi/org-typst-preview
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Simple library that allows to preview Typst fragments inside Emacs buffer.
;;
;; Every typst code fragment should be in "#[typst code]" block. This syntax was
;; chosen in favor formula dollar signs and it allows to work on every written
;; formula in buffer. This allows to have function that work on the whole buffer.
;;
;; See `org-typst-preview-render-buffer' and `org-typst-preview-clear-buffer'
;;
;;; Code:

(require 'color)
(require 'seq)
(require 'org)
(require 'cl-lib)

(defun org-typst-preview--select-formula ()
  "Select typst formula under cursor.

Returns t if this is block formula, nil otherwise."
  (interactive)
  (let (beg-space end-space)
    (and (equal ?$ (following-char)) (forward-char))
    (while (progn
             (search-backward "$")
             (backward-char)
             (equal ?\\ (following-char))))
    (forward-char)
    (set-mark (point))
    (forward-char)
    (setq beg-space (equal ?\s (following-char)))
    (while (progn
             (search-forward "$")
             (backward-char)
             (prog1 (equal ?\\ (preceding-char))
               (forward-char))))

    (backward-char)
    (setq end-space (equal ?\s (preceding-char)))
    (forward-char)

    (and beg-space end-space)))

(make-obsolete 'org-typst-preview--select-formula
               'org-typst-preview--select-code-block "ever")

(defun org-typst-preview--opening-forward ()
  "Return t if opening of code block forward from cursor."
  ;; Escape \#[
  (and (not (char-equal ?\\ (preceding-char)))
       (char-equal ?\# (following-char))
       (eq ?\[ (char-after (+ (point) 1)))))

(defun org-typst-preview--closing-forward ()
  "Return t if closing of code block forward from cursor."
  ;; Escape \#]
  (and (not (char-equal ?\\ (preceding-char)))
       (char-equal ?\# (following-char))
       (eq ?\] (char-after (+ (point) 1)))))


(defun org-typst-preview--all-code-blocks ()
  "Scan buffer and return list pairs of format (BEG . END).

Note that list in reverse order."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((spans '())
          (beg 0))
      (while (not (eobp))
        (cond
         ((and (eq beg 0) (org-typst-preview--opening-forward))
          (setq beg (point)))
         ((and (not (eq beg 0)) (org-typst-preview--closing-forward))
          (push (cons beg (+ (point) 2)) spans)
          (setq beg 0)))
        (forward-char))
      spans)))

;; TODO: Memoize to remove necessity to walk over all buffer every time.
(defun org-typst-preview--select-code-block ()
  "Select typst code block under cursor in form #[some typst code]."
  (interactive)
  (let* ((spans (org-typst-preview--all-code-blocks))
         (pos (point))
         (span (seq-find
                (lambda (p) (<= (car p) pos (1- (cdr p))))
                spans)))
    ;; REVIEW: Maybe push-mark?
    (set-mark (car span))
    (goto-char (cdr span))))

(defun org-typst-preview--render-image (typst-file-path image-file-path)
  "Generate svg image from TYPST-FILE-PATH to IMAGE-FILE-PATH."
  ;; TODO: Replace with `start-process' and join them to enable massive parallel
  ;; render.
  (shell-command (format "typst compile -f svg %s %s"
                         typst-file-path
                         image-file-path)))

(defun org-typst-preview--typst-foreground-color ()
  "Gets current theme foreground color and reformat it for typst."
  ;; (cdr (assq 'foreground-color (frame-parameters)))
  (let* ((foreground-color (face-foreground 'default))
         (hex-color (apply 'color-rgb-to-hex (append (color-name-to-rgb foreground-color) '(2)))))
    (format "rgb(%S)" hex-color)))

(defun org-typst-preview--typst-font-settings ()
  "Return typst additional font settings.
Those are based on `default' face font.
Currently passes weight and size."
  (let* ((font (face-attribute 'default :font))
         (weight (symbol-name (font-get font :weight)))
         (size (font-get font :size))
         (options `(("weight" . ,(format "%S" weight))
                    ("size" . ,(format "%dpt" size)))))
    ;; ;; TODO: Think what to do to display fonts better
    ;; ("top-edge" . ,(format "%S" "ascender"))
    ;; ("bottom-edge" . ,(format "%S" "descender"))
    (mapconcat (lambda (p) (format "%s: %s" (car p) (cdr p))) options ", ")))

(defun org-typst-preview--generate-typst-file (file-path typst-code &optional common-configuration)
  "Generate typst file at FILE-PATH with TYPST-CODE and COMMON-CONFIGURATION."
  (with-temp-file file-path
    (insert "#set page(fill: none, width: auto, height: auto, margin: (x: 0pt, y: 0pt))\n") ;; , margin: (x: 20pt, y: 20pt)
    (insert "#show math.equation: set text(top-edge: \"bounds\", bottom-edge: \"bounds\")\n")
    (insert (format "#set text(fill: %s, %s)\n"
                    (org-typst-preview--typst-foreground-color)
                    (org-typst-preview--typst-font-settings)))
    (insert-char ?\n)
    (insert (or common-configuration "// Your configuration\n"))
    (insert-char ?\n)
    (insert (substring typst-code 2 -2))))

(defun org-typst-preview--generate-svg-image (typst-code &optional dir)
  "Generate image for TYPST-CODE and return path to it.
When REPLACE and DIR non-nil"
  (let* ((dir (or dir (make-temp-file "org-typst-preview-" t)))
         (typst-file-path (expand-file-name "main.typ" dir))
         (image-file-path (expand-file-name "main.svg" dir)))
    (org-typst-preview--generate-typst-file typst-file-path typst-code)
    (and (= 0 (org-typst-preview--render-image typst-file-path image-file-path))
         image-file-path)))

;; TODO: Disable image when cursor on image and rerender after it exists area.
;;       Like in https://github.com/karthink/org-preview
(defun org-typst-preview-format (beg end)
  "Display preview at BEG END span."
  (let ((image-path (org-typst-preview--generate-svg-image (buffer-substring beg end)))
        (ov (make-overlay beg end)))
    (overlay-put ov 'org-overlay-type 'org-typst-overlay)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'modification-hooks
                 (list (lambda (o _flag _beg _end &optional _l)
                         (delete-overlay o))))
    (overlay-put ov 'display
                 `(image :type svg :file ,image-path :ascent center))))

;; Manage overlays
(defun org-typst-preview--overlays-in-range (beg end)
  "Return org typst overlays in range from BEG to END."
  (interactive "r")
  (seq-filter (lambda (ov) (eq (overlay-get ov 'org-overlay-type) 'org-typst-overlay))
              (overlays-in beg end)))

(defun org-typst-preview--overlay-image-path (ov)
  "Extract image path from OV."
  (plist-get (cdr (overlay-get ov 'display)) :file))

;; FIXME: In some unknown corner cases directories are steal leaking, but this
;;        may be due to constant change of code during development.
(defun org-typst-preview--remove-overlay (ov)
  "Remove typst preview OV and directory with image."
  (let* ((image-path (org-typst-preview--overlay-image-path ov))
         (temp-dir (file-name-directory (or image-path "./."))))
    (delete-overlay ov)
    ;; Don't delete directory if there's none.
    (and image-path (delete-directory temp-dir t))))

(defun org-typst-preview--remove-overlays-in-range (beg end)
  "Remove typst preview overlays in range from BEG to END.

Returns true if some overlays were removed."
  (interactive "r")

  (let ((ovs (org-typst-preview--overlays-in-range beg end)))
    (dolist (ov ovs)
      (org-typst-preview--remove-overlay ov))
    (consp ovs)))

(defun org-typst-preview--remove-overlay-under-cursor ()
  "Remove typst preview overlay under cursor."
  (interactive)
  ;; Use `max' to handle corner case when cursor in the beginning of the buffer.
  (org-typst-preview--remove-overlays-in-range (max 1 (1- (point))) (1+ (point))))

;; Preview
(defun org-typst-preview--region (beg end)
  "Preview Typst fragments between BEG and END.
BEG and END are buffer positions."
  (unless (org-typst-preview--remove-overlays-in-range beg end)
    (org-typst-preview-format beg end)))

(defun org-typst-preview ()
  "Toggle preview of the Typst fragment at point.

If the cursor is on a Typst fragment, create the image and
overlay it over the source code.  Remove it
otherwise.

When there's active region render or remove it instead."
  (interactive)
  (cond
   ((not (display-graphic-p)) nil)
   (t (save-mark-and-excursion
        (unless (use-region-p)
          (org-typst-preview--select-code-block))
        (org-typst-preview--region (region-beginning) (region-end))))))

(defun org-typst-preview-clear-buffer ()
  "Remove all Typst code blocks rendered images in buffer."
  (interactive)
  (org-typst-preview--remove-overlays-in-range (point-min) (point-max)))

(defun org-typst-preview-render-buffer ()
  "Render all (unrendered) Typst code blocks in buffer."
  (interactive)
  (dolist (typst-code-block (org-typst-preview--all-code-blocks))
    (let* ((beg (car typst-code-block))
           (end (cdr typst-code-block))
           (middle (/ (+ beg end) 2)))
      (unless (consp (overlays-at middle))
        (org-typst-preview--region beg end)))))

;; Hooks to keep images in sync
(defun org-typst-preview--rerender ()
  "Rerender all typst svg images in current buffer."
  (interactive)
  (save-excursion
    (dolist (ov (org-typst-preview--overlays-in-range (point-min) (point-max)))
      (let ((beg (overlay-start ov))
            (end (overlay-end ov)))
        (goto-char beg)
        (org-typst-preview--remove-overlay-under-cursor)
        (org-typst-preview--region beg end)))))

(defun org-typst-preview-rerender-all-org-buffers (&rest _)
  "Match theme in Typst generated images.
Run rerender on every theme change."
  (interactive)
  (dolist (org-buf (org-buffer-list))
    (with-current-buffer org-buf
      (org-typst-preview--rerender))))

(add-hook 'after-setting-font-hook #'org-typst-preview-rerender-all-org-buffers)

;; TODO: There's a lot of issues with advices, mostly because of timings of
;;       loading things. So trigger rerender manually on theme change.
;;
;; ;; REVIEW: Add noticable theme change lag, but usually this shouldn't be an issue.
;; ;; Could be solved by running rerender in separate thread.
;; (advice-add 'load-theme :after #'org-typst-preview--rerender-all-org-buffers)
;; (advice-add 'enable-theme :after #'org-typst-preview--rerender-all-org-buffers)
;; (advice-add 'disable-theme :after #'org-typst-preview--rerender-all-org-buffers)

;; Bindings
(org-defkey org-mode-map (kbd "C-c C-x C-t") #'org-typst-preview)
(org-defkey org-mode-map (kbd "C-c C-x t t") #'org-typst-preview-render-buffer)
(org-defkey org-mode-map (kbd "C-c C-x t c") #'org-typst-preview-clear-buffer)

(provide 'org-typst-preview)
;;; org-typst-preview.el ends here
