(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;* Straight config
(setq straight-use-package-by-default t)
(setq straight-host-usernames '((github . "cheerio-pixel")))

(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))
;; ---------------------------------------------------------------------------------------------


(use-package ejc-sql
  :config
  (setq ejc-connections
        '(("Asig2"
           (:classname . "com.mysql.jdbc.Driver")
           (:classpath .
                       ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
           (:password . "testtest")
           (:user . "MySQL test")
           (:port . "3306")
           (:host . "localhost")
           (:dbname . "Asig2")
           (:dbtype . "mysql"))
          ("mysql1"
           (:classname . "com.mysql.jdbc.Driver")
           (:classpath .
                       ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
           (:password . "testtest")
           (:user . "MySQL test")
           (:port . "3306")
           (:host . "localhost")
           (:dbname . "intro2")
           (:dbtype . "mysql"))
          ("lab3"
           (:classname . "com.mysql.jdbc.Driver")
           (:classpath .
                       ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
           (:password . "testtest")
           (:user . "MySQL test")
           (:port . "3306")
           (:host . "localhost")
           (:dbname . "lab3")
           (:dbtype . "mysql"))
          ("Group"
           (:classname . "com.mysql.jdbc.Driver")
           (:classpath .
                       ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
           (:password . "testtest")
           (:user . "MySQL test")
           (:port . "3306")
           (:host . "localhost")
           (:dbname . "Tarea2Group")
           (:dbtype . "mysql"))
          ("Final"
           (:classname . "com.mysql.jdbc.Driver")
           (:classpath .
                       ["/home/cheerio-pixel/.m2/repository/mysql/mysql-connector-java/5.1.44/mysql-connector-java-5.1.44.jar"])
           (:password . "testtest")
           (:user . "MySQL test")
           (:port . "3306")
           (:host . "localhost")
           (:dbname . "ProyectoFinal")
           (:dbtype . "mysql"))
          ))
  )

