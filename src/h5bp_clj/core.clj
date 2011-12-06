(ns h5bp-clj.core
  (:use hiccup.core
        hiccup.page-helpers))

(defn- html-comment [cmt]
  (str "<!--" cmt "-->"))

(defn- ie-cond-comment
  "Takes a version number or a collection with an operation keyword and a version and returns an IE conditional comment"
  [version & body]
  (html-comment
   (apply str "[if " (if (coll? version)
                       (str (name (first version)) " IE " (second version))
                       (str "IE " version))
          "]>" (html body) "<![endif]")))

(defn- ie-check [version]
  (let [msg #(str "<html class=\"no-js ie" % " oldie\" lang=\"en\">")]
    (cond
     (< version 7) (ie-cond-comment [:lt 7] (msg 6))
     (or (= version 7) (= version 8)) (ie-cond-comment version (msg version))
     (> version 8) (ie-cond-comment [:gt 8]
                                    "<!--><html class=\"no-js\" lang=\"en\"><!--"))))

(defn h5bp [options & content]
  "See h5bp.com for information on HTML5Boilerplate\n\n
content should be in the form of:\n
<header>...</header>\n
<div id=\"main\" role=\"main\">...</div>\n
<footer>...</footer>\n\n
CSS and JS should be minified through using the build script, and so all CSS and JS will be pulled from css/style.css, js/plugins.js, and js/script.js\n\n
Options are: title, description, author, goog-site-id (Google Analytics), modernizr-path, respond-path, jquery-path"
  (html
   (doctype :html5)
   (map ie-check [6 7 8 9])
   [:head
    [:meta {:charset "utf-8"}]
                                        ; DNS prefetches here?
    [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1"}]
    [:title (:title options)]
    [:meta {:name "description" :content (:description options)}]
    [:meta {:name "author" :content (:author options)}]
    [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
    (include-css "css/style.css?v=2") ;put in docs to use only style.css
    (include-js (if-let [path (:modernizr-path options)] path "http://cdnjs.cloudflare.com/ajax/libs/modernizr/2.0.6/modernizr.min.js"))
    (if-let [path (:respond-path options)] (include-js path))]
   [:body
    [:div#container (html content)]
    (include-js "http://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js")
    [:script "window.jQuery||document.write('<script src=\""
     (if-let [path (get options :jquery-path)] path "js/libs/jquery-1.6.2.min.js") "\">\\x3C/script>')"]
    (include-js "js/plugins.js" "js/script.js") ;put in documentation
                                        ;that javascript should live
                                        ;here
    [:script (str "window._gaq=[['_setAccount','" (if-let [id (:goog-site-id options)] id "UAXXXXXXXX1") "'],['_trackPageview'],['_trackPageLoadTime']];Modernizr.load({load: ('https:' == location.protocol ? '//ssl' : '//www') + '.google-analytics.com/ga.js'});")]
    (ie-cond-comment [:lt 7]
                     [:script {:src "http://ajax.googleapis.com/ajax/libs/chrome-frame/1.0.3/CFInstall.min.js"}]
                     [:script "window.attachEvent('onload',function(){CFInstall.check({mode:'overlay'})})"])
    ]
   "</html>"))
