;; Multi-Send Contract with Pause Functionality
;; Allows batch STX transfers with emergency pause controls

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-contract-paused (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-invalid-recipient (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-transfer-failed (err u105))
(define-constant err-empty-recipients (err u106))

;; Data Variables
(define-data-var contract-paused bool false)
(define-data-var total-transfers uint u0)

;; Data Maps
(define-map authorized-operators principal bool)

;; Private Functions

;; Check if contract is paused
(define-private (is-paused)
  (var-get contract-paused)
)

;; Check if caller is contract owner
(define-private (is-owner (caller principal))
  (is-eq caller contract-owner)
)

;; Check if caller is authorized operator
(define-private (is-authorized-operator (caller principal))
  (default-to false (map-get? authorized-operators caller))
)

;; Check if caller can perform admin functions
(define-private (can-admin (caller principal))
  (or (is-owner caller) (is-authorized-operator caller))
)

;; Validate transfer parameters
(define-private (validate-transfer (recipient principal) (amount uint))
  (and 
    (> amount u0)
    (not (is-eq recipient tx-sender))
  )
)

;; Execute single STX transfer
(define-private (execute-transfer (recipient principal) (amount uint))
  (begin
    (asserts! (validate-transfer recipient amount) err-invalid-recipient)
    (match (stx-transfer? amount tx-sender recipient)
      success (ok true)
      error err-transfer-failed
    )
  )
)

;; Process single transfer for fold operation
(define-private (process-single-transfer (transfer-data {recipient: principal, amount: uint}) (previous-result (response uint uint)))
  (match previous-result
    prev-success (match (execute-transfer (get recipient transfer-data) (get amount transfer-data))
                   transfer-success (ok prev-success)
                   transfer-error (err transfer-error))
    prev-error (err prev-error)
  )
)

;; Read-Only Functions

;; Get contract pause status
(define-read-only (get-pause-status)
  (var-get contract-paused)
)

;; Get total number of transfers processed
(define-read-only (get-total-transfers)
  (var-get total-transfers)
)

;; Check if address is authorized operator
(define-read-only (is-operator (operator principal))
  (default-to false (map-get? authorized-operators operator))
)

;; Get contract owner
(define-read-only (get-contract-owner)
  contract-owner
)

;; Check if caller can perform admin operations
(define-read-only (can-perform-admin (caller principal))
  (can-admin caller)
)

;; Public Functions

;; Pause the contract (owner/operator only)
(define-public (pause-contract)
  (begin
    (asserts! (can-admin tx-sender) err-owner-only)
    (var-set contract-paused true)
    (ok true)
  )
)

;; Unpause the contract (owner/operator only)
(define-public (unpause-contract)
  (begin
    (asserts! (can-admin tx-sender) err-owner-only)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Add authorized operator (owner only)
(define-public (add-operator (operator principal))
  (begin
    (asserts! (is-owner tx-sender) err-owner-only)
    (map-set authorized-operators operator true)
    (ok true)
  )
)

;; Remove authorized operator (owner only)
(define-public (remove-operator (operator principal))
  (begin
    (asserts! (is-owner tx-sender) err-owner-only)
    (map-delete authorized-operators operator)
    (ok true)
  )
)

;; Multi-send STX to multiple recipients
(define-public (multi-send-stx (recipients (list 50 {recipient: principal, amount: uint})))
  (let (
    (total-amount (fold + (map get-amount recipients) u0))
    (recipient-count (len recipients))
  )
    ;; Check if contract is paused
    (asserts! (not (is-paused)) err-contract-paused)
    
    ;; Check if recipients list is not empty
    (asserts! (> recipient-count u0) err-empty-recipients)
    
    ;; Check if sender has sufficient balance
    (asserts! (>= (stx-get-balance tx-sender) total-amount) err-insufficient-balance)
    
    ;; Process all transfers using fold
    (match (fold process-single-transfer recipients (ok u0))
      success (begin
        ;; Update total transfers counter
        (var-set total-transfers (+ (var-get total-transfers) recipient-count))
        (ok {
          total-sent: total-amount,
          recipients-count: recipient-count,
          transfer-id: (var-get total-transfers)
        })
      )
      error (err error)
    )
  )
)

;; Helper function to extract amount from recipient tuple
(define-private (get-amount (recipient-data {recipient: principal, amount: uint}))
  (get amount recipient-data)
)

;; Emergency withdrawal function (owner only) - for stuck funds
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-owner tx-sender) err-owner-only)
    (asserts! (is-paused) err-contract-paused) ;; Can only withdraw when paused
    (stx-transfer? amount (as-contract tx-sender) contract-owner)
  )
)

;; Multi-send with custom memo
(define-public (multi-send-stx-with-memo 
  (recipients (list 50 {recipient: principal, amount: uint})) 
  (memo (buff 34)))
  (let (
    (total-amount (fold + (map get-amount recipients) u0))
    (recipient-count (len recipients))
  )
    ;; Check if contract is paused
    (asserts! (not (is-paused)) err-contract-paused)
    
    ;; Check if recipients list is not empty
    (asserts! (> recipient-count u0) err-empty-recipients)
    
    ;; Check if sender has sufficient balance
    (asserts! (>= (stx-get-balance tx-sender) total-amount) err-insufficient-balance)
    
    ;; Process all transfers using fold
    (match (fold process-single-transfer recipients (ok u0))
      success (begin
        ;; Update total transfers counter
        (var-set total-transfers (+ (var-get total-transfers) recipient-count))
        ;; Print memo for transaction tracking
        (print {memo: memo, batch-id: (var-get total-transfers)})
        (ok {
          total-sent: total-amount,
          recipients-count: recipient-count,
          transfer-id: (var-get total-transfers),
          memo: memo
        })
      )
      error (err error)
    )
  )
)