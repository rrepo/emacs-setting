;; パッケージ管理
(require 'package)
(setq package-archives
    '(("melpa" . "https://melpa.org/packages/")
      ("gnu" . "https://elpa.gnu.org/packages/")
      ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; use-package のインストール
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; 基本設定
(setq inhibit-startup-message t) ;; スタートアップメッセージを非表示
(setq make-backup-files nil) ;; バックアップファイルを作成しない
(setq auto-save-default nil) ;; 標準の自動保存を無効化
(global-display-line-numbers-mode t) ;; 行番号を表示

(use-package auto-save-buffers-enhanced
             :ensure t
             :config
             (setq auto-save-buffers-enhanced-interval 1)
             (setq auto-save-buffers-enhanced-quiet-save-p t)
             (auto-save-buffers-enhanced t))


(defun dotspacemacs/user-config ()
  ;; auto save buffers enhanced settings
  (require 'auto-save-buffers-enhanced)
  (setq auto-save-buffers-enhanced-interval 1)
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  (auto-save-buffers-enhanced t))

;; ivy 設定
(use-package ivy
             :ensure t
             :bind (("C-s" . swiper)
                    ("C-c C-r" . ivy-resume)
                    ("M-x" . counsel-M-x)
                    ("C-x C-f" . counsel-find-file))
             :config
             (ivy-mode 1))

;; magit 設定
(use-package magit
             :ensure t
             :bind (("C-x g" . magit-status)))

;; which-key 設定
(use-package which-key
             :ensure t
             :config
             (which-key-mode))

;; SLIME 設定
(use-package slime
             :ensure t
             :config
             ;; 使用する Lisp 実装（ここでは SBCL）を指定
             (setq inferior-lisp-program "/opt/homebrew/bin/sbcl") ;; SBCL のパスを指定
             ;; Emacs 起動時に SLIME を自動的に接続する
             (add-hook 'emacs-startup-hook #'slime))

(defun my-slime-startup-hook ()
  "SLIME起動後にウィンドウを非表示にする"
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (let ((lisp-buffer (get-buffer "*inferior-lisp*")))
       (if lisp-buffer
           (delete-window (get-buffer-window lisp-buffer)))))))

(add-hook 'slime-connected-hook 'my-slime-startup-hook)


;; rainbow-delimiters パッケージを読み込む
(use-package rainbow-delimiters
             :ensure t
             :hook (prog-mode . rainbow-delimiters-mode)) ;; プログラムモードで有効にする

;; テーマ設定
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'jonadabian-slate t)

;; カスタム関数
(defun open-init-file ()
  "初期化ファイルを開く関数。"
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c I") 'open-init-file)

;; キーマッピング
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c r") 'replace-string)

;; インデント設定
(setq-default indent-tabs-mode nil) ;; タブをスペースに変換
(setq-default tab-width 2) ;; タブ幅を2に設定
(setq indent-line-function 'insert-tab)

;; スペルチェック
(use-package flyspell
             :ensure t
             :config
             (add-hook 'text-mode-hook 'flyspell-mode)
             (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package all-the-icons
             :ensure t)

;; ファイルツリー
(use-package neotree
             :ensure t
             :bind (("C-c n" . neotree-toggle)) ;; ショートカットキー
             :config
             (setq neo-window-fixed-size nil) ;; ウィンドウサイズを可変に設定
             (setq neo-smart-open t)
             (global-set-key [f8] 'neotree-toggle)
             (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
(add-hook 'emacs-startup-hook 'neotree-toggle)

;; 補完機能
(use-package slime
             :ensure t
             :config
             (setq inferior-lisp-program "sbcl") ;; Lispインタプリタを指定
             (setq slime-contribs '(slime-fancy)) ;; SLIME拡張機能を有効化
             (setq slime-complete-symbol*-fancy t) ;; 高度な補完機能を有効化
             (setq slime-completion-at-point-functions '(slime-fuzzy-complete-symbol)) ;; 補完機能をSLIMEのファジー補完に設定
             ; (define-key slime-mode-map (kbd "TAB") 'slime-complete-symbol) ;; TABキーで補完を有効化
             ;; slime-repl-mode-mapの設定は slime-repl のロード後に行う
             (with-eval-after-load 'slime-repl
                                   (define-key slime-repl-mode-map (kbd "TAB") 'slime-complete-symbol)))


;; slime-companyの設定
(use-package slime-company
             :ensure t
             :after (slime company)
             :config
             (setq slime-company-completion 'fuzzy) ;; ファジー補完を有効化
             (setq slime-company-completion-limit 20) ;; 補完候補の最大数
             (add-to-list 'company-backends 'company-slime)) ;; companyバックエンドにslimeを追加

;; Companyの設定
(use-package company
             :ensure t
             :config
             (global-company-mode) ;; 全バッファで補完を有効化
             (setq company-idle-delay 0.2) ;; 補完を開始するまでの待ち時間
             (setq company-minimum-prefix-length 1) ;; 補完を開始する文字数
             (setq company-tooltip-align-annotations t) ;; 補完候補の説明を整列
             (setq company-backends '(company-slime company-files company-dabbrev)))


;; シンタックスハイライト
(global-font-lock-mode t)

(electric-pair-mode 1)

;; 行末の空白を可視化
(setq show-trailing-whitespace t)

;; カーソルを線
(add-to-list 'default-frame-alist '(cursor-type . bar))
(blink-cursor-mode -1)

;; マウス操作の有効化
(xterm-mouse-mode 1)

;; 80文字目にラインを表示
; (use-package display-fill-column-indicator
;   :ensure t
;   :config
;   (setq display-fill-column-indicator-column 80)
;   (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(format-all hiwin dashboard org-bullets git-gutter flymake-posframe posframe ivy-rich rainbow-delimiters flycheck display-fill-column-indicator company doom-themes which-key magit ivy slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package flycheck
             :ensure t
             :config
             (global-flycheck-mode))

(use-package flycheck
             :ensure t
             :config
             (global-flycheck-mode))

(use-package rainbow-delimiters
             :ensure t
             :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
             :ensure t
             :config
             (global-company-mode))

(use-package posframe
             :ensure t)

(package-refresh-contents)

(use-package git-gutter
             :ensure t ;; インストールを確実に行う
             :custom
             (git-gutter:modified-sign "~")
             (git-gutter:added-sign "+")
             (git-gutter:deleted-sign "-")
             :custom-face
             (git-gutter:modified ((t (:background "#f1fa8c"))))
             (git-gutter:added ((t (:background "#50fa7b"))))
             (git-gutter:deleted ((t (:background "#ff79c6"))))
             :config
             (global-git-gutter-mode +1))

(defun ladicle/task-clocked-time ()
  "Return a string with the clocked time and effort, if any"
  (interactive)
  (let* ((clocked-time (org-clock-get-clocked-time))
         (h (truncate clocked-time 60))
         (m (mod clocked-time 60))
         (work-done-str (format "%d:%02d" h m)))
    (if org-clock-effort
        (let* ((effort-in-minutes
                (org-duration-to-minutes org-clock-effort))
               (effort-h (truncate effort-in-minutes 60))
               (effort-m (truncate (mod effort-in-minutes 60)))
               (effort-str (format "%d:%02d" effort-h effort-m)))
          (format "%s/%s" work-done-str effort-str))
        (format "%s" work-done-str))))

(use-package org-bullets
             :ensure t
             :hook (org-mode . org-bullets-mode))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; 起動画面の変更
(use-package dashboard
             :ensure t
             :config
             (dashboard-setup-startup-hook)
             ;; ASCIIアートバナーを指定
             (setq dashboard-startup-banner "~/.emacs.d/ascii-banner.txt")
             ;; ロゴタイトルを非表示にする（任意）
             (setq dashboard-banner-logo-title nil))

;; bufferの最後でカーソルを動かそうとしても音をならなくする
(defun next-line (arg)
  (interactive "p")
  (condition-case nil
                  (line-move arg)
                  (end-of-buffer)))

;; エラー音をならなくする
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-j")
                (lambda ()
                  (interactive)
                  (split-window-below) ;; 下に新しいウィンドウを作成
                  (other-window 1) ;; 作成したウィンドウに移動
                  (eshell))) ;; eshellを起動

(when (display-graphic-p)
      (require 'all-the-icons))
;; or
(use-package all-the-icons
             :if (display-graphic-p))

(global-set-key (kbd "s-/") 'comment-line)

;; 括弧の対応不足やエラーをハイライト表示
(use-package paren
             :ensure nil
             :config
             (show-paren-mode 1) ;; 対応する括弧をハイライト
             (setq show-paren-delay 0) ;; ハイライトの遅延時間をゼロに
             (setq show-paren-style 'mixed) ;; 括弧のスタイル（'parenthesis, 'expression, 'mixed）
             (set-face-attribute 'show-paren-match nil
                                 :background "#44475a" ;; 対応する括弧の背景色
                                 :foreground "#f8f8f2"
                                 :weight 'bold)
             (set-face-attribute 'show-paren-mismatch nil
                                 :background "#ff5555" ;; 対応しない括弧の背景色
                                 :foreground "#ffffff"
                                 :weight 'bold))

(delete-selection-mode 1)
(electric-indent-mode 1)

;; whitespace-mode 設定
(use-package whitespace
             :ensure nil
             :config
             (setq whitespace-style '(face spaces tabs newline space-mark tab-mark newline-mark))
             (setq whitespace-display-mappings
                 '((space-mark ?\u3000 [?\u25a1]) ;; 全角スペースを□で表示
                                                 (space-mark ?\ [?.] [?.]) ;; 半角スペースを.で表示
                                                 (newline-mark ?\n [?\u21b5 ?\n] [?$ ?\n]) ;; 改行記号を↵で表示
                                                 (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))) ;; タブを»で表示
             (global-whitespace-mode 1)) ;; 全てのバッファで有効化

;; スペースとタブの表示色を設定
(set-face-attribute 'whitespace-space nil
                    :background nil
                    :foreground "gray50"
                    :weight 'bold)
(set-face-attribute 'whitespace-tab nil
                    :background nil
                    :foreground "gray35")
(set-face-attribute 'whitespace-newline nil
                    :background nil
                    :foreground "gray20")

(defun slime-format-buffer ()
  "SLIME を使用してバッファ全体をフォーマットします。
- 不要な空白を削除（カッコ内外）
- 行末の空白削除
- 適切なインデント適用
- コメントの整列"
  (interactive)
  (when (derived-mode-p 'lisp-mode 'slime-repl-mode)
        (save-excursion
         (goto-char (point-min))

         ;; 1. 開きカッコの後の不要な空白を削除
         (while (re-search-forward "\\([(\[]\\)[ \t]+" nil t)
                (replace-match "\\1"))

         ;; 2. 閉じカッコの前の不要な空白を削除
         (goto-char (point-min))
         (while (re-search-forward "[ \t]+\\([\)\]]\\)" nil t)
                (replace-match "\\1"))

         ;; 3. 行末の空白を削除
         (goto-char (point-min))
         (while (re-search-forward "[ \t]+$" nil t)
                (replace-match ""))

         ;; 4. 複数の空行を1つにまとめる
         (goto-char (point-min))
         (while (re-search-forward "\n\\{3,\\}" nil t)
                (replace-match "\n\n"))

         ;; 5. コメントの整列
         (goto-char (point-min))
         (while (re-search-forward "^[ \t]*;+" nil t)
                (indent-for-tab-command))

         ;; 6. SLIME を使った全体の再インデント
         (goto-char (point-min))
         (indent-region (point-min) (point-max))

         ;; 7. 各 `defun` ブロックの再インデント
         (goto-char (point-min))
         (while (not (eobp))
                (when (not (looking-at-p "\\s-*$"))
                      (slime-reindent-defun))
                (forward-sexp)))))

;; キーバインド設定
(global-set-key (kbd "M-F") 'slime-format-buffer)

(global-set-key (kbd "M-f") 'forward-word)

(global-set-key (kbd "s-<return>") 'eval-last-sexp)

;; Ctrl + Backspace で行末まで削除
(defun kill-to-beginning-of-line ()
  "Delete text from the cursor to the beginning of the line."
  (interactive)
  (delete-region (point) (line-beginning-position)))
(global-set-key (kbd "s-<backspace>") 'kill-to-beginning-of-line)

;; Alt + 矢印キーで括弧単位で移動
(global-set-key (kbd "M-<right>") 'forward-sexp)
(global-set-key (kbd "M-<left>") 'backward-sexp)
(global-set-key (kbd "M-<down>") 'down-list)
(global-set-key (kbd "M-<up>") 'backward-up-list)

;; undo+ と redo+ を使用
(require 'redo+)
;; Ctrl + z で Undo
(global-set-key (kbd "C-z") 'undo)
;; Ctrl + Shift + z で Redo（複数回Redoを可能にする）
(global-set-key (kbd "C-S-z") 'redo)
;; Ctrl + y で Redoを行わないようにする
(global-set-key (kbd "C-y") 'undefined)
(defun custom-redo-only (orig-fun &rest args)
  "Ctrl + Shift + ZでRedoを繰り返す動作に限定する"
  (unless (or (eq last-command 'undo) (eq last-command 'redo))
    (apply orig-fun args)))
(advice-add 'redo :around 'custom-redo-only)

;; Grepをプロジェクト全体で使用
(global-set-key (kbd "C-c p f") 'project-find-file) ;; プロジェクト内ファイルを検索して開く
(global-set-key (kbd "C-c p g") 'project-find-regexp) ;; プロジェクト内で正規表現検索
(global-set-key (kbd "C-c p d") 'project-switch-project) ;; プロジェクトを切り替える
