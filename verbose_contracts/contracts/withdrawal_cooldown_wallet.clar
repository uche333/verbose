;; Withdrawal Cooldown Wallet Smart Contract
;; This contract implements a wallet with cooldown periods between withdrawals

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-COOLDOWN-NOT-PASSED (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-INVALID-COOLDOWN (err u104))

;; Default cooldown period (24 hours in blocks, assuming ~10 minute blocks)
(define-constant DEFAULT-COOLDOWN-BLOCKS u144)

;; Data Variables
(define-data-var cooldown-period uint DEFAULT-COOLDOWN-BLOCKS)

;; Data Maps
;; Track user balances
(define-map user-balances principal uint)

;; Track last withdrawal time for each user
(define-map last-withdrawal-time principal uint)

;; Read-only functions
(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-last-withdrawal-time (user principal))
  (default-to u0 (map-get? last-withdrawal-time user))
)

(define-read-only (get-cooldown-period)
  (var-get cooldown-period)
)

(define-read-only (can-withdraw (user principal))
  (let ((last-withdrawal (get-last-withdrawal-time user))
        (current-block block-height)
        (cooldown (var-get cooldown-period)))
    (>= (- current-block last-withdrawal) cooldown)
  )
)

(define-read-only (blocks-until-next-withdrawal (user principal))
  (let ((last-withdrawal (get-last-withdrawal-time user))
        (current-block block-height)
        (cooldown (var-get cooldown-period))
        (blocks-passed (- current-block last-withdrawal)))
    (if (>= blocks-passed cooldown)
        u0
        (- cooldown blocks-passed)
    )
  )
)

;; Public functions

;; Deposit STX into the wallet
(define-public (deposit (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (let ((current-balance (get-balance tx-sender)))
      (map-set user-balances tx-sender (+ current-balance amount))
      (ok amount)
    )
  )
)

;; Withdraw STX from the wallet (with cooldown check)
(define-public (withdraw (amount uint))
  (let ((user-balance (get-balance tx-sender)))
    ;; Check if amount is valid
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Check if user has sufficient balance
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
    ;; Check if cooldown period has passed
    (asserts! (can-withdraw tx-sender) ERR-COOLDOWN-NOT-PASSED)
    
    ;; Perform withdrawal
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update user balance
    (map-set user-balances tx-sender (- user-balance amount))
    
    ;; Update last withdrawal time
    (map-set last-withdrawal-time tx-sender block-height)
    
    (ok amount)
  )
)

;; Emergency withdraw (only for contract owner, bypasses cooldown)
(define-public (emergency-withdraw (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (let ((user-balance (get-balance user)))
      (asserts! (> amount u0) ERR-INVALID-AMOUNT)
      (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
      
      (try! (as-contract (stx-transfer? amount user tx-sender)))
      (map-set user-balances user (- user-balance amount))
      
      (ok amount)
    )
  )
)

;; Set cooldown period (only contract owner)
(define-public (set-cooldown-period (new-cooldown uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (> new-cooldown u0) ERR-INVALID-COOLDOWN)
    (var-set cooldown-period new-cooldown)
    (ok new-cooldown)
  )
)

;; Get contract STX balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)