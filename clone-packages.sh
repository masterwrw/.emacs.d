!/bin/bash
mkdir -p ~/.emacs.d/packages
cd ~/.emacs.d/packages

git clone https://github.com/abo-abo/hydra.git --depth=1
git clone https://github.com/Fanael/rainbow-delimiters.git --depth=1
git clone https://github.com/Fanael/highlight-numbers.git --depth=1
git clone https://github.com/nschum/idle-require.el.git idle-require
git clone https://github.com/immerrr/lua-mode.git  --depth=1
git clone https://github.com/joaotavora/yasnippet.git  --depth=1
git clone https://gitlab.com/vancan1ty/emacs-backward-forward.git  --depth=1
git clone https://github.com/magnars/dash.el.git dash  --depth=1
git clone https://github.com/emacs-helm/helm.git --depth=1
git clone https://github.com/areina/helm-dash.git --depth=1
git clone https://github.com/Wilfred/helpful.git --depth=1
git clone https://github.com/magit/magit.git  --depth=1
git clone https://github.com/emacs-php/php-mode.git  --depth=1
git clone https://github.com/xuchunyang/youdao-dictionary.el.git youdao-dictionary  --depth=1
git clone https://github.com/EricCrosson/qt-pro-mode.git  --depth=1
git clone https://github.com/cataska/qml-mode.git  --depth=1
git clone https://github.com/cute-jumper/company-qml.git  --depth=1
git clone https://github.com/redguardtoo/counsel-etags.git  --depth=1
git clone https://github.com/jacktasia/dumb-jump.git  --depth=1
git clone https://github.com/paradoxxxzero/jinja2-mode.git  --depth=1
git clone https://github.com/emacsmirror/rainbow-mode.git  --depth=1
git clone https://github.com/nflath/hungry-delete.git  --depth=1
git clone https://github.com/mhayashi1120/Emacs-wgrep.git  --depth=1
git clone https://github.com/justbur/emacs-which-key.git  --depth=1
git clone https://github.com/abo-abo/avy.git  --depth=1
git clone https://github.com/magnars/multiple-cursors.el.git multiple-cursors  --depth=1
git clone https://github.com/enoson/eno.el.git eno  --depth=1
git clone https://github.com/joostkremers/writeroom-mode.git  --depth=1
git clone https://github.com/jwiegley/emacs-async.git  --depth=1
git clone https://github.com/abo-abo/swiper.git  --depth=1
git clone https://github.com/nonsequitur/smex.git  --depth=1
git clone https://github.com/technomancy/find-file-in-project.git  --depth=1
git clone https://github.com/company-mode/company-mode.git  --depth=1
git clone https://github.com/randomphrase/company-c-headers.git  --depth=1
git clone https://github.com/zenitani/elisp.git  --depth=1
git clone https://github.com/pitkali/pos-tip.git  --depth=1
git clone https://github.com/xuchunyang/chinese-word-at-point.el.git chinese-word-at-point  --depth=1
git clone https://github.com/auto-complete/popup-el.git popup  --depth=1
git clone https://github.com/Malabarba/names.git  --depth=1
git clone https://github.com/hniksic/emacs-htmlize.git  --depth=1
git clone https://github.com/magnars/s.el.git s  --depth=1
git clone https://github.com/rejeep/f.el.git f  --depth=1
git clone https://github.com/enoson/edit-at-point.el.git edit-at-point  --depth=1
git clone https://github.com/joostkremers/visual-fill-column.git  --depth=1
git clone https://github.com/magit/ghub.git  --depth=1
git clone https://github.com/vermiculus/graphql.el.git graphql  --depth=1
git clone https://github.com/volrath/treepy.el.git treepy  --depth=1
git clone https://github.com/magit/with-editor.git  --depth=1
git clone https://github.com/magit/magit-popup.git  --depth=1
git clone https://github.com/magit/transient.git --depth=1
git clone https://github.com/xcwen/ac-php.git  --depth=1
git clone https://github.com/bbatsov/projectile.git  --depth=1
git clone https://github.com/manateelazycat/color-rg.git  --depth=1
git clone https://github.com/tumashu/company-posframe.git  --depth=1
git clone https://github.com/tuhdo/semantic-refactor.git  --depth=1
git clone https://github.com/tumashu/posframe.git  --depth=1
git clone https://github.com/Wilfred/elisp-refs.git  --depth=1
git clone https://github.com/Wilfred/loop.el.git loop  --depth=1
git clone https://github.com/cask/shut-up.git  --depth=1
git clone https://github.com/kuanyui/moe-theme.el.git moe-theme  --depth=1
git clone https://github.com/manateelazycat/aweshell.git  --depth=1
git clone https://github.com/bbatsov/super-save.git  --depth=1
git clone https://github.com/fxbois/web-mode.git  --depth=1
git clone https://github.com/Kungsgeten/yankpad.git  --depth=1
git clone https://github.com/zargener/emacs-password-genarator.git  --depth=1
git clone https://github.com/manateelazycat/watch-other-window.git  --depth=1
git clone https://github.com/tumashu/ivy-posframe.git  --depth=1
git clone https://github.com/dajva/rg.el.git rg  --depth=1
git clone https://github.com/bbatsov/solarized-emacs.git  --depth=1
git clone https://github.com/jordonbiondo/ample-theme.git  --depth=1
git clone https://github.com/skeeto/elfeed.git  --depth=1
git clone https://gitlab.com/bennya/shrink-path.el.git shrink-path  --depth=1
git clone https://github.com/thierryvolpiatto/eldoc-eval.git  --depth=1
git clone https://github.com/skeeto/emacs-memoize.git  --depth=1
git clone https://github.com/domtronn/all-the-icons.el.git all-the-icons  --depth=1
git clone https://github.com/seagle0128/doom-modeline.git  --depth=1
git clone https://github.com/hlissner/emacs-hide-mode-line.git  --depth=1
git clone https://github.com/hlissner/emacs-doom-themes.git  --depth=1
git clone https://github.com/hlissner/emacs-solaire-mode.git  --depth=1
git clone https://github.com/cute-jumper/avy-zap.git  --depth=1

mkdir -p find-file-in-tags
wget -O./find-file-in-tags/find-file-in-tags.el http://www.emacswiki.org/emacs/download/find-file-in-tags.el

mkdir -p tempbuf
wget -O./tempbuf/tempbuf.el http://www.emacswiki.org/emacs/download/tempbuf.el
git clone https://github.com/expez/company-quickhelp.git  --depth=1
git clone https://github.com/joodland/bm.git  --depth=1
git clone https://github.com/jrblevin/deft.git  --depth=1
git clone https://github.com/bastibe/org-journal.git  --depth=1
git clone https://github.com/mkcms/ivy-yasnippet.git  --depth=1
git clone https://github.com/wolray/symbol-overlay.git  --depth=1
git clone https://github.com/joddie/pcre2el.git  --depth=1
git clone https://github.com/soeye/org-wiki.git  --depth=1
git clone https://github.com/nickav/naysayer-theme.el.git naysayer-theme  --depth=1
git clone https://github.com/dacap/keyfreq.git  --depth=1
git clone https://github.com/mattiase/xr.git  --depth=1
git clone https://github.com/tumashu/pyim.git  --depth=1
git clone https://github.com/yefeiyu/pyim-wbdict.git  --depth=1
git clone https://github.com/owensys/global-readonly.git --depth=1

# evil
git clone https://github.com/emacs-evil/goto-chg.git --depth=1
git clone https://github.com/emacs-evil/evil.git --depth=1

# terminal theme
git clone https://github.com/kuanyui/moe-theme.el.git moe-theme --depth=1

git clone https://github.com/editorconfig/editorconfig-emacs.git editorconfig

git clone https://github.com/redguardtoo/org2nikola.git

# theme
git clone https://github.com/srcery-colors/srcery-emacs.git

git clone https://github.com/manateelazycat/awesome-tab.git
git clone https://github.com/manateelazycat/awesome-tray.git
git clone https://github.com/Fanael/snails.git

git clone https://github.com/Fanael/nox.git
git clone https://github.com/Fanael/parent-mode.git

# notdeft
git clone https://github.com/hasu/notdeft.git
sudo apt install libtclap-dev libxapian-dev
pushd notdeft/xapian
make
popd

# apt-utils
mkdir -p apt-utils
pushd apt-utils
wget http://www.emacswiki.org/emacs/download/apt-utils.el
popd

# org-sidebar
git clone https://github.com/alphapapa/org-super-agenda.git
git clone https://github.com/alphapapa/org-sidebar.git
git clone https://github.com/alphapapa/org-ql.git
git clone https://github.com/alphapapa/ts.el.git
git clone https://github.com/emacsmirror/peg.git
git clone https://github.com/emacsmirror/ht.git
git clone https://github.com/emacsmirror/ov.git

# dashboard
git clone https://github.com/emacs-dashboard/emacs-dashboard.git
git clone https://github.com/purcell/page-break-lines.git

# dict
git clone https://github.com/cute-jumper/bing-dict.el.git bing-dict

# org-roam
git clone https://github.com/org-roam/org-roam.git
git clone https://github.com/skeeto/emacsql.git
git clone https://github.com/cireu/emacsql-sqlite3.git

# auto insert space
git clone https://github.com/coldnew/pangu-spacing.git

git clone https://github.com/DogLooksGood/meow.git

# gkroam
git clone https://github.com/Kinneyzhang/gkroam.git
git clone https://github.com/nicferrier/emacs-db.git
git clone https://github.com/nicferrier/emacs-kv.git

# hugo
git clone https://github.com/kaushalmodi/ox-hugo.git


git clone https://github.com/Yevgnen/ivy-rich.git

# eaf
git clone https://github.com/kiwanami/emacs-ctable.git
git clone https://github.com/kiwanami/emacs-deferred.git
git clone https://github.com/kiwanami/emacs-epc.git
git clone https://github.com/manateelazycat/emacs-application-framework.git
