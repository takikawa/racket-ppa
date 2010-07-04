#lang web-server/insta

;; A blog is a (make-blog posts)
;; where posts is a (listof post)
(define-struct blog (posts) #:mutable)

;; and post is a (make-post title body comments)
;; where title is a string, body is a string,
;; and comments is a (listof string)
(define-struct post (title body comments) #:mutable)

;; BLOG: blog
;; The initial BLOG.
(define BLOG 
  (make-blog
   (list (make-post "First Post" 
                    "This is my first post" 
                    (list "First comment!"))
         (make-post "Second Post" 
                    "This is another post"
                    (list)))))

;; blog-insert-post!: blog post -> void
;; Consumes a blog and a post, adds the post at the top of the blog.
(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))


;; post-insert-comment!: post string -> void
;; Consumes a post and a comment string.  As a side-efect, 
;; adds the comment to the bottom of the post's list of comments.
(define (post-insert-comment! a-post a-comment)
  (set-post-comments!
   a-post
   (append (post-comments a-post) (list a-comment))))

;; start: request -> html-response
;; Consumes a request, and produces a page that displays
;; all of the web content.
(define (start request)
  (render-blog-page request))

;; render-blog-page: request -> html-response
;; Produces an html-response page of the content of the
;; BLOG.
(define (render-blog-page request)
  (local [(define (response-generator make-url)        
            `(html (head (title "My Blog"))
                   (body 
                    (h1 "My Blog")
                    ,(render-posts make-url)
                    (form ((action 
                            ,(make-url insert-post-handler)))
                     (input ((name "title")))
                     (input ((name "body")))
                     (input ((type "submit")))))))
          
          ;; parse-post: bindings -> post
          ;; Extracts a post out of the bindings.
          (define (parse-post bindings)
            (make-post (extract-binding/single 'title bindings)
                       (extract-binding/single 'body bindings)
                       (list)))
          
          (define (insert-post-handler request)
            (blog-insert-post!
             BLOG (parse-post (request-bindings request)))
            (render-blog-page request))]

    (send/suspend/dispatch response-generator)))

;; render-post-detail-page: post request -> html-response
;; Consumes a post and request, and produces a detail page
;; of the post. The user will be able to insert new comments.
(define (render-post-detail-page a-post request)
  (local [(define (response-generator make-url)
            `(html (head (title "Post Details"))
                   (body
                    (h1 "Post Details")
                    (h2 ,(post-title a-post))
                    (p ,(post-body a-post))
                    ,(render-as-itemized-list
                      (post-comments a-post))
                    (form ((action 
                            ,(make-url insert-comment-handler)))
                          (input ((name "comment")))
                          (input ((type "submit")))))))

          (define (parse-comment bindings)
            (extract-binding/single 'comment bindings))
          
          (define (insert-comment-handler a-request)
            (post-insert-comment! 
             a-post (parse-comment (request-bindings a-request)))
            (render-post-detail-page a-post a-request))]


    (send/suspend/dispatch response-generator)))


;; render-post: post (handler -> string) -> html-response
;; Consumes a post, produces an html-response fragment of the post.
;; The fragment contains a link to show a detailed view of the post.
(define (render-post a-post make-url)
  (local [(define (view-post-handler request)
            (render-post-detail-page a-post request))]
    `(div ((class "post")) 
          (a ((href ,(make-url view-post-handler)))
             ,(post-title a-post))
          (p ,(post-body a-post))        
          (div ,(number->string (length (post-comments a-post)))
               " comment(s)"))))

;; render-posts: (handler -> string) -> html-response
;; Consumes a make-url, and produces an html-response fragment
;; of all its posts.
(define (render-posts make-url)
  (local [(define (render-post/make-url a-post)
            (render-post a-post make-url))]
    `(div ((class "posts"))
          ,@(map render-post/make-url (blog-posts BLOG)))))

;; render-as-itemized-list: (listof html-response) -> html-response
;; Consumes a list of items, and produces a rendering as
;; an unorderered list.
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

;; render-as-item: html-response -> html-response
;; Consumes an html-response, and produces a rendering
;; as a list item.
(define (render-as-item a-fragment)
  `(li ,a-fragment))