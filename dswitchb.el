;;; dswitchb.el --- select buffer more directly

;; Author: Akira Kitauchi
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;; [TODO] mod: カレントバッファが*scratch*などのときは最初dswitchb-buffer-listの先頭に切り替える
;; [TODO] mod: minibufferのバッファ名強調表示色を文字色青、背景白に
;; [TODO] add: パッケージ化
;; [TODO] fix: マッチするバッファがないときに2つのバッファが交互に表示される
;; [TODO] mod: refactoring: define-minor-modeを使う
;; [TODO] mod: 名前(dswitchb)の変更(dはdirectのdのつもりだがイマイチ)
;; [TODO] mod: バッファの絞り込みをcompleting-readで行う?

;; iswitchbとの違い
;; - 切替中のバッファがリアルタイムで表示されるのでスムーズ
;; - 切替モードを任意の制御文字で抜けられるのでスムーズ
;; - バッファ一覧がminibufferではなく別バッファに縦に表示されるので見やすい

(defvar dswitchb-mode nil)
(defvar dswitchb-buffer-index nil)
(defvar dswitchb-buffer-list nil)
(defvar dswitchb-buffer-searched-list nil)
(defvar dswitchb-search-string "")
(defvar dswitchb-window-configuration nil)
(defvar dswitchb-buffer-name "*dswitchb*")

(eval-when-compile (require 'cl))

(defvar dswitchb-mode-map
  (let ((i 0)
	(map (make-keymap)))
    (while (< i ?\s)
      (define-key map (make-string 1 i) 'dswitchb-other-meta-char)
      (setq i (1+ i)))
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'dswitchb-printing-char)
      (setq i (1+ i)))
    (let ((meta-map (make-sparse-keymap)))
      (define-key map (char-to-string meta-prefix-char) meta-map)
      (define-key map [escape] meta-map))
    (define-key map (vector meta-prefix-char t) 'dswitchb-other-meta-char)
    (define-key map "\177" 'dswitchb-delete-char)
    (define-key map "\C-g" 'dswitchb-quit)
    (define-key map "\r" 'dswitchb-done)
    (define-key map "\M-\C-j" 'dswitchb-repeat-forward)
    (define-key map "\M-\C-k" 'dswitchb-repeat-backward)
    map)
  "Keymap for `dswitchb'.")

(or (assq 'dswitchb-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(dswitchb-mode dswitchb-mode))))

(defun dswitchb-mode ()
  (setq	dswitchb-mode " Dswitchb")
  (force-mode-line-update)
  (setq overriding-local-map dswitchb-mode-map)
  (run-hooks 'dswitchb-hook))

(defun dswitchb-forward ()
  (interactive)
  (setq dswitchb-window-configuration (current-window-configuration))
  (dswitchb-list-buffers)
  (dswitchb-next-buffer))

(defun dswitchb-backward ()
  (interactive)
  (setq dswitchb-window-configuration (current-window-configuration))
  (dswitchb-list-buffers)
  (dswitchb-previous-buffer))

(defun dswitchb-quit ()
  (interactive)
  (let ((buf (nth 0 dswitchb-buffer-list)))
    (switch-to-buffer buf t))
  (dswitchb-done))

(defun dswitchb-done ()
  (interactive)
  (setq	dswitchb-mode nil)
  (force-mode-line-update)
  (setq overriding-local-map nil)
  (let ((bufname (buffer-name (nth dswitchb-buffer-index dswitchb-buffer-searched-list))))
    (set-text-properties 0 (length bufname) nil bufname))
  (kill-buffer dswitchb-buffer-name)
  (let ((buf (current-buffer)))
    (when dswitchb-window-configuration
      (set-window-configuration dswitchb-window-configuration))
    (switch-to-buffer buf)))

(defun dswitchb-list-buffers ()
  (setq dswitchb-buffer-list nil)
  (let ((buflist (buffer-list)))
    (dolist (buf buflist)
      (if (not (find (aref (buffer-name buf) 0) " *"))
	(setq dswitchb-buffer-list
	      (append dswitchb-buffer-list (list buf))))))
  (setq dswitchb-search-string "")
  (update-dswitchb-buffer-searched-list))

(defun update-dswitchb-buffer-searched-list ()
  (setq dswitchb-buffer-index 0)
  (setq dswitchb-buffer-searched-list nil)
  (dolist (buf dswitchb-buffer-list)
    (if (string-match dswitchb-search-string (buffer-name buf))
	(setq dswitchb-buffer-searched-list
	      (append dswitchb-buffer-searched-list (list buf))))))

(defun dswitchb-other-meta-char ()
  (interactive)
  (dswitchb-done)
  (let* ((key (this-command-keys))
	 (keylist (listify-key-sequence key)))
    (apply 'dswitchb-unread keylist)))

(defun dswitchb-delete-char ()
  (interactive)
  (setq dswitchb-search-string
	(replace-regexp-in-string ".$" "" dswitchb-search-string))
  (update-dswitchb-buffer-searched-list)
  (dswitchb-update-buffer)
  (dswitchb-switch-to-current-buffer))

(defun dswitchb-printing-char ()
  (interactive)
  (let* ((char last-command-event)
	 (string (char-to-string char)))
    (setq dswitchb-search-string (concat dswitchb-search-string string)))
  (update-dswitchb-buffer-searched-list)
  (dswitchb-update-buffer)
  (dswitchb-switch-to-current-buffer))

(defun dswitchb-unread (&rest char-or-events)
  (mapc 'store-kbd-macro-event char-or-events)
  (setq unread-command-events
	(append char-or-events unread-command-events)))

(defun dswitchb-repeat-forward ()
  (interactive)
  (dswitchb-next-buffer))

(defun dswitchb-repeat-backward ()
  (interactive)
  (dswitchb-previous-buffer))

(defun dswitchb-next-buffer ()
  (setq dswitchb-buffer-index
	(if (eq dswitchb-buffer-index
		(1- (length dswitchb-buffer-searched-list)))
	    0
	  (1+ dswitchb-buffer-index)))
  (dswitchb-switch-to-current-buffer)
  (dswitchb-mode))

(defun dswitchb-previous-buffer ()
  (setq dswitchb-buffer-index
	(if (eq dswitchb-buffer-index 0)
	    (1- (length dswitchb-buffer-searched-list))
	  (1- dswitchb-buffer-index)))
  (dswitchb-switch-to-current-buffer)
  (dswitchb-mode))

(defun dswitchb-switch-to-current-buffer ()
  (let ((buf (nth dswitchb-buffer-index dswitchb-buffer-searched-list)))
    (switch-to-buffer buf t))
  (dswitchb-update-buffer))

(defun dswitchb-update-buffer ()
  (let ((n (length dswitchb-buffer-searched-list))
	(bufstr "")
	(i 0))
    (while (< i n)
      (let* ((bufname (buffer-name (nth i dswitchb-buffer-searched-list)))
	     (face (and (eq i dswitchb-buffer-index) '(face region))))
	(set-text-properties 0 (length bufname) face bufname)
	(setq bufstr (concat bufstr bufname "\n")))
      (setq i (1+ i)))
    (if (eq bufstr "") (setq bufstr "(no match)"))
    (dswitchb-display-buffer bufstr)
    (message "pattern: %s" dswitchb-search-string)))

(defun dswitchb-display-buffer (bufstr)
  (display-buffer
   (with-current-buffer (get-buffer-create dswitchb-buffer-name)
     (setq buffer-read-only nil)
     (erase-buffer)
     (insert bufstr)
     (current-buffer))))

(global-set-key "\M-\C-j" 'dswitchb-forward)
(global-set-key "\M-\C-k" 'dswitchb-backward)
