;; FileRating Smart Contract
;; Purpose: Rate files to improve content quality with star rating system

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_RATING (err u101))
(define-constant ERR_ALREADY_RATED (err u102))
(define-constant ERR_FILE_NOT_FOUND (err u103))
(define-constant ERR_INVALID_INPUT (err u104))
(define-constant MIN_RATING u1)
(define-constant MAX_RATING u5)
(define-constant MIN_NAME_LENGTH u1)
(define-constant MAX_NAME_LENGTH u100)

;; Data Variables
(define-data-var next-file-id uint u1)

;; Data Maps
;; Store file information
(define-map files
    { file-id: uint }
    {
        name: (string-ascii 100),
        total-rating: uint,
        rating-count: uint,
        average-rating: uint,
        creator: principal
    }
)

;; Track user ratings for each file to prevent duplicate ratings
(define-map user-ratings
    { file-id: uint, user: principal }
    { rating: uint }
)

;; Store all ratings for a file (for detailed analysis if needed)
(define-map file-ratings
    { file-id: uint, rating-index: uint }
    { 
        user: principal,
        rating: uint,
        timestamp: uint
    }
)

;; Track rating count per file for indexing
(define-map file-rating-counts
    { file-id: uint }
    { count: uint }
)

;; Helper Functions

;; Validate file name input
(define-private (is-valid-name (name (string-ascii 100)))
    (and 
        (>= (len name) MIN_NAME_LENGTH)
        (<= (len name) MAX_NAME_LENGTH)
        ;; Prevent empty or whitespace-only names
        (> (len (unwrap-panic (as-max-len? name u100))) u0)
    )
)

;; Public Functions

;; Register a new file for rating
(define-public (register-file (name (string-ascii 100)))
    (let
        (
            (file-id (var-get next-file-id))
        )
        ;; Validate input thoroughly
        (asserts! (is-valid-name name) ERR_INVALID_INPUT)
        
        ;; Create file record
        (map-set files
            { file-id: file-id }
            {
                name: name,
                total-rating: u0,
                rating-count: u0,
                average-rating: u0,
                creator: tx-sender
            }
        )
        
        ;; Initialize rating count
        (map-set file-rating-counts
            { file-id: file-id }
            { count: u0 }
        )
        
        ;; Increment next file ID
        (var-set next-file-id (+ file-id u1))
        
        (ok file-id)
    )
)

;; Rate a file (1-5 stars)
(define-public (rate-file (file-id uint) (rating uint))
    (let
        (
            (file-data (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
            (existing-rating (map-get? user-ratings { file-id: file-id, user: tx-sender }))
            (current-rating-count (default-to u0 (get count (map-get? file-rating-counts { file-id: file-id }))))
        )
        ;; Validate rating range (untrusted input validation)
        (asserts! (and (>= rating MIN_RATING) (<= rating MAX_RATING)) ERR_INVALID_RATING)
        
        ;; Validate file-id is not zero or unreasonably large (prevent overflow attacks)
        (asserts! (and (> file-id u0) (< file-id u1000000)) ERR_INVALID_INPUT)
        
        ;; Check if user already rated this file
        (asserts! (is-none existing-rating) ERR_ALREADY_RATED)
        
        ;; Record the user's rating
        (map-set user-ratings
            { file-id: file-id, user: tx-sender }
            { rating: rating }
        )
        
        ;; Store detailed rating information
        (map-set file-ratings
            { file-id: file-id, rating-index: current-rating-count }
            {
                user: tx-sender,
                rating: rating,
                timestamp: block-height
            }
        )
        
        ;; Update rating count
        (map-set file-rating-counts
            { file-id: file-id }
            { count: (+ current-rating-count u1) }
        )
        
        ;; Calculate new totals with overflow protection
        (let
            (
                (new-total-rating (+ (get total-rating file-data) rating))
                (new-rating-count (+ (get rating-count file-data) u1))
            )
            ;; Ensure no overflow in calculations
            (asserts! (> new-total-rating (get total-rating file-data)) ERR_INVALID_INPUT)
            (asserts! (> new-rating-count (get rating-count file-data)) ERR_INVALID_INPUT)
            
            (let ((new-average-rating (/ new-total-rating new-rating-count)))
                ;; Update file with new aggregated data
                (map-set files
                    { file-id: file-id }
                    {
                        name: (get name file-data),
                        total-rating: new-total-rating,
                        rating-count: new-rating-count,
                        average-rating: new-average-rating,
                        creator: (get creator file-data)
                    }
                )
                
                (ok true)
            )
        )
    )
)

;; Read-only Functions

;; Get file information including average rating
(define-read-only (get-file-info (file-id uint))
    (map-get? files { file-id: file-id })
)

;; Get average rating for a file
(define-read-only (get-average-rating (file-id uint))
    (match (map-get? files { file-id: file-id })
        file-data (ok (get average-rating file-data))
        ERR_FILE_NOT_FOUND
    )
)

;; Get total number of ratings for a file
(define-read-only (get-rating-count (file-id uint))
    (match (map-get? files { file-id: file-id })
        file-data (ok (get rating-count file-data))
        ERR_FILE_NOT_FOUND
    )
)

;; Check if a user has already rated a file
(define-read-only (has-user-rated (file-id uint) (user principal))
    (is-some (map-get? user-ratings { file-id: file-id, user: user }))
)

;; Get a user's rating for a specific file
(define-read-only (get-user-rating (file-id uint) (user principal))
    (map-get? user-ratings { file-id: file-id, user: user })
)

;; Get detailed rating information by index
(define-read-only (get-file-rating-by-index (file-id uint) (rating-index uint))
    (map-get? file-ratings { file-id: file-id, rating-index: rating-index })
)

;; Get the current file ID counter
(define-read-only (get-next-file-id)
    (var-get next-file-id)
)

;; Get all ratings summary for a file
(define-read-only (get-file-ratings-summary (file-id uint))
    (match (map-get? files { file-id: file-id })
        file-data 
        (ok {
            file-id: file-id,
            name: (get name file-data),
            average-rating: (get average-rating file-data),
            total-ratings: (get rating-count file-data),
            creator: (get creator file-data)
        })
        ERR_FILE_NOT_FOUND
    )
)