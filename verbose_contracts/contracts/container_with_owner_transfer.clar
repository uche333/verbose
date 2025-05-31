;; Enhanced Storage with Owner Transfer Contract
;; A comprehensive smart contract with advanced storage, ownership, and access control features

;; Define constants for error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-ADDRESS (err u101))
(define-constant ERR-SAME-OWNER (err u102))
(define-constant ERR-ALREADY-INITIALIZED (err u103))
(define-constant ERR-CONTRACT-PAUSED (err u104))
(define-constant ERR-INVALID-VALUE (err u105))
(define-constant ERR-ADMIN-NOT-FOUND (err u106))
(define-constant ERR-ALREADY-ADMIN (err u107))
(define-constant ERR-CANNOT-REMOVE-OWNER (err u108))
(define-constant ERR-TRANSFER-COOLDOWN (err u109))
(define-constant ERR-VALUE-TOO-LOW (err u110))
(define-constant ERR-VALUE-TOO_HIGH (err u111))
(define-constant ERR-UNAUTHORIZED-ADMIN (err u112))

;; Define data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var stored-value uint u0)
(define-data-var contract-paused bool false)
(define-data-var min-value uint u0)
(define-data-var max-value uint u1000000)
(define-data-var last-transfer-block uint u0)
(define-data-var transfer-cooldown uint u144) ;; ~24 hours in blocks
(define-data-var total-updates uint u0)
(define-data-var contract-version (string-ascii 10) "1.0.0")

;; Define maps for advanced features
(define-map admins principal bool)
(define-map authorized-updaters principal bool)
(define-map value-history uint {value: uint, updater: principal, block-height: uint})
(define-map user-permissions principal {can-read: bool, can-update: bool})
(define-map pending-ownership-transfers principal {proposed-owner: principal, expires-at: uint})

;; Define fungible token for rewards (optional feature)
(define-fungible-token storage-points)

;; Define read-only functions

;; Get the current contract owner
(define-read-only (get-owner)
  (var-get contract-owner)
)

;; Get the current stored value
(define-read-only (get-value)
  (var-get stored-value)
)

;; Check if the caller is the contract owner
(define-read-only (is-owner (caller principal))
  (is-eq caller (var-get contract-owner))
)

;; Check if an address is an admin
(define-read-only (is-admin (address principal))
  (default-to false (map-get? admins address))
)

;; Check if an address is an authorized updater
(define-read-only (is-authorized-updater (address principal))
  (default-to false (map-get? authorized-updaters address))
)

;; Get contract status information
(define-read-only (get-contract-info)
  {
    owner: (var-get contract-owner),
    paused: (var-get contract-paused),
    total-updates: (var-get total-updates),
    current-value: (var-get stored-value),
    min-value: (var-get min-value),
    max-value: (var-get max-value),
    version: (var-get contract-version),
    last-transfer-block: (var-get last-transfer-block)
  }
)

;; Get value history entry
(define-read-only (get-value-history (index uint))
  (map-get? value-history index)
)

;; Get user permissions
(define-read-only (get-user-permissions (user principal))
  (default-to {can-read: false, can-update: false} (map-get? user-permissions user))
)

;; Check if contract is paused
(define-read-only (is-paused)
  (var-get contract-paused)
)

;; Get pending ownership transfer
(define-read-only (get-pending-transfer (current-owner principal))
  (map-get? pending-ownership-transfers current-owner)
)

;; Calculate value statistics (last 10 entries)
(define-read-only (get-value-stats)
  (let ((current-updates (var-get total-updates)))
    (if (> current-updates u0)
      (let ((start-index (if (> current-updates u10) (- current-updates u10) u0))
            (showing-count (if (< current-updates u10) current-updates u10)))
        {
          total-entries: current-updates,
          showing-last: showing-count
        }
      )
      {total-entries: u0, showing-last: u0}
    )
  )
)

;; Define private functions

;; Internal function to validate that caller is the owner
(define-private (assert-owner)
  (if (is-eq tx-sender (var-get contract-owner))
    (ok true)
    ERR-NOT-AUTHORIZED
  )
)

;; Internal function to validate that caller is owner or admin
(define-private (assert-owner-or-admin)
  (if (or (is-eq tx-sender (var-get contract-owner)) (is-admin tx-sender))
    (ok true)
    ERR-NOT-AUTHORIZED
  )
)

;; Internal function to check if contract is not paused
(define-private (assert-not-paused)
  (if (var-get contract-paused)
    ERR-CONTRACT-PAUSED
    (ok true)
  )
)

;; Internal function to validate value range
(define-private (validate-value-range (value uint))
  (if (and (>= value (var-get min-value)) (<= value (var-get max-value)))
    (ok true)
    (if (< value (var-get min-value))
      ERR-VALUE-TOO-LOW
      ERR-VALUE-TOO_HIGH
    )
  )
)

;; Internal function to record value in history
(define-private (record-value-history (value uint))
  (let ((update-count (var-get total-updates)))
    (begin
      (map-set value-history update-count {
        value: value,
        updater: tx-sender,
        block-height: block-height
      })
      (var-set total-updates (+ update-count u1))
    )
  )
)

;; Define public functions

;; === CORE STORAGE FUNCTIONS ===

;; Update the stored value (owner, admin, or authorized updater can call this)
(define-public (update-value (new-value uint))
  (begin
    ;; Check if contract is not paused
    (try! (assert-not-paused))
    ;; Check authorization
    (asserts! (or 
      (is-eq tx-sender (var-get contract-owner))
      (is-admin tx-sender)
      (is-authorized-updater tx-sender)
    ) ERR-NOT-AUTHORIZED)
    ;; Validate value range
    (try! (validate-value-range new-value))
    ;; Record in history
    (record-value-history new-value)
    ;; Update the stored value
    (var-set stored-value new-value)
    ;; Mint reward points to updater
    (try! (ft-mint? storage-points u10 tx-sender))
    ;; Log the update
    (print {
      event: "value-updated",
      old-value: (var-get stored-value),
      new-value: new-value,
      updated-by: tx-sender,
      block-height: block-height
    })
    (ok true)
  )
)

;; Batch update multiple values (average them)
(define-public (batch-update-values (values (list 10 uint)))
  (let ((sum (fold + values u0))
        (count (len values))
        (average (/ sum count)))
    (begin
      (try! (assert-not-paused))
      (try! (assert-owner-or-admin))
      (try! (validate-value-range average))
      (record-value-history average)
      (var-set stored-value average)
      (print {
        event: "batch-value-updated",
        values: values,
        average: average,
        updated-by: tx-sender
      })
      (ok average)
    )
  )
)

;; === OWNERSHIP MANAGEMENT ===

;; Propose ownership transfer (two-step process for safety)
(define-public (propose-ownership-transfer (new-owner principal))
  (begin
    (try! (assert-owner))
    (asserts! (not (is-eq new-owner (var-get contract-owner))) ERR-SAME-OWNER)
    ;; Check transfer cooldown
    (asserts! (> block-height (+ (var-get last-transfer-block) (var-get transfer-cooldown))) ERR-TRANSFER-COOLDOWN)
    ;; Set pending transfer (expires in 1008 blocks ~1 week)
    (map-set pending-ownership-transfers (var-get contract-owner) {
      proposed-owner: new-owner,
      expires-at: (+ block-height u1008)
    })
    (print {
      event: "ownership-transfer-proposed",
      current-owner: (var-get contract-owner),
      proposed-owner: new-owner,
      expires-at: (+ block-height u1008)
    })
    (ok true)
  )
)

;; Accept ownership transfer
(define-public (accept-ownership-transfer)
  (let ((pending (map-get? pending-ownership-transfers (var-get contract-owner))))
    (match pending
      transfer-data (if (and 
                         (is-eq tx-sender (get proposed-owner transfer-data))
                         (< block-height (get expires-at transfer-data)))
                       (begin
                         (var-set contract-owner tx-sender)
                         (var-set last-transfer-block block-height)
                         (map-delete pending-ownership-transfers (var-get contract-owner))
                         (print {
                           event: "ownership-transferred",
                           old-owner: (var-get contract-owner),
                           new-owner: tx-sender
                         })
                         (ok true)
                       )
                       ERR-NOT-AUTHORIZED)
      ERR-NOT-AUTHORIZED
    )
  )
)

;; Cancel pending ownership transfer
(define-public (cancel-ownership-transfer)
  (begin
    (try! (assert-owner))
    (map-delete pending-ownership-transfers (var-get contract-owner))
    (print {event: "ownership-transfer-cancelled", owner: (var-get contract-owner)})
    (ok true)
  )
)

;; Emergency ownership transfer (immediate, for emergencies only)
(define-public (emergency-transfer-ownership (new-owner principal))
  (begin
    (try! (assert-owner))
    (asserts! (not (is-eq new-owner (var-get contract-owner))) ERR-SAME-OWNER)
    (var-set contract-owner new-owner)
    (var-set last-transfer-block block-height)
    (print {
      event: "emergency-ownership-transferred",
      old-owner: tx-sender,
      new-owner: new-owner,
      reason: "emergency"
    })
    (ok true)
  )
)

;; === ADMIN MANAGEMENT ===

;; Add an admin
(define-public (add-admin (new-admin principal))
  (begin
    (try! (assert-owner))
    (asserts! (not (is-admin new-admin)) ERR-ALREADY-ADMIN)
    (map-set admins new-admin true)
    (print {event: "admin-added", admin: new-admin, added-by: tx-sender})
    (ok true)
  )
)

;; Remove an admin
(define-public (remove-admin (admin principal))
  (begin
    (try! (assert-owner))
    (asserts! (not (is-eq admin (var-get contract-owner))) ERR-CANNOT-REMOVE-OWNER)
    (map-delete admins admin)
    (print {event: "admin-removed", admin: admin, removed-by: tx-sender})
    (ok true)
  )
)

;; === AUTHORIZED UPDATERS MANAGEMENT ===

;; Add authorized updater
(define-public (add-authorized-updater (updater principal))
  (begin
    (try! (assert-owner-or-admin))
    (map-set authorized-updaters updater true)
    (print {event: "authorized-updater-added", updater: updater, added-by: tx-sender})
    (ok true)
  )
)

;; Remove authorized updater
(define-public (remove-authorized-updater (updater principal))
  (begin
    (try! (assert-owner-or-admin))
    (map-delete authorized-updaters updater)
    (print {event: "authorized-updater-removed", updater: updater, removed-by: tx-sender})
    (ok true)
  )
)

;; === CONTRACT CONTROL ===

;; Pause the contract
(define-public (pause-contract)
  (begin
    (try! (assert-owner-or-admin))
    (var-set contract-paused true)
    (print {event: "contract-paused", paused-by: tx-sender})
    (ok true)
  )
)

;; Unpause the contract
(define-public (unpause-contract)
  (begin
    (try! (assert-owner-or-admin))
    (var-set contract-paused false)
    (print {event: "contract-unpaused", unpaused-by: tx-sender})
    (ok true)
  )
)

;; Set value range limits
(define-public (set-value-range (min-val uint) (max-val uint))
  (begin
    (try! (assert-owner-or-admin))
    (asserts! (< min-val max-val) ERR-INVALID-VALUE)
    (var-set min-value min-val)
    (var-set max-value max-val)
    (print {
      event: "value-range-updated",
      min-value: min-val,
      max-value: max-val,
      updated-by: tx-sender
    })
    (ok true)
  )
)

;; Set transfer cooldown period
(define-public (set-transfer-cooldown (blocks uint))
  (begin
    (try! (assert-owner))
    (var-set transfer-cooldown blocks)
    (print {event: "transfer-cooldown-updated", blocks: blocks})
    (ok true)
  )
)

;; === USER PERMISSIONS ===

;; Set user permissions
(define-public (set-user-permissions (user principal) (can-read bool) (can-update bool))
  (begin
    (try! (assert-owner-or-admin))
    (map-set user-permissions user {can-read: can-read, can-update: can-update})
    (print {
      event: "user-permissions-updated",
      user: user,
      can-read: can-read,
      can-update: can-update,
      updated-by: tx-sender
    })
    (ok true)
  )
)

;; === REWARD SYSTEM ===

;; Claim reward points (for active users)
(define-public (claim-daily-reward)
  (begin
    (try! (assert-not-paused))
    ;; Simple daily reward of 5 points
    (try! (ft-mint? storage-points u5 tx-sender))
    (print {event: "daily-reward-claimed", user: tx-sender, amount: u5})
    (ok true)
  )
)

;; Transfer reward points between users
(define-public (transfer-points (recipient principal) (amount uint))
  (begin
    (try! (ft-transfer? storage-points amount tx-sender recipient))
    (print {
      event: "points-transferred",
      from: tx-sender,
      to: recipient,
      amount: amount
    })
    (ok true)
  )
)

;; Get user's point balance
(define-read-only (get-point-balance (user principal))
  (ft-get-balance storage-points user)
)

;; === UTILITY FUNCTIONS ===

;; Increment value by amount
(define-public (increment-value (amount uint))
  (let ((new-value (+ (var-get stored-value) amount)))
    (update-value new-value)
  )
)

;; Decrement value by amount
(define-public (decrement-value (amount uint))
  (let ((current (var-get stored-value)))
    (if (>= current amount)
      (let ((new-value (- current amount)))
        (update-value new-value)
      )
      (update-value u0)
    )
  )
)

;; Reset value to zero (emergency function)
(define-public (reset-value)
  (begin
    (try! (assert-owner))
    (record-value-history u0)
    (var-set stored-value u0)
    (print {event: "value-reset", reset-by: tx-sender})
    (ok true)
  )
)

;; Update contract version
(define-public (update-version (new-version (string-ascii 10)))
  (begin
    (try! (assert-owner))
    (var-set contract-version new-version)
    (print {event: "version-updated", version: new-version})
    (ok true)
  )
)

;; === INITIALIZATION ===

;; Initialize function with comprehensive setup
(define-public (initialize (initial-value uint) (min-val uint) (max-val uint))
  (begin
    ;; Only allow initialization if value is still 0
    (asserts! (is-eq (var-get stored-value) u0) ERR-ALREADY-INITIALIZED)
    (try! (assert-owner))
    ;; Set initial configuration
    (var-set stored-value initial-value)
    (var-set min-value min-val)
    (var-set max-value max-val)
    ;; Record initial value in history
    (record-value-history initial-value)
    ;; Make owner an admin by default
    (map-set admins (var-get contract-owner) true)
    (print {
      event: "contract-initialized",
      initial-value: initial-value,
      min-value: min-val,
      max-value: max-val,
      owner: (var-get contract-owner)
    })
    (ok true)
  )
)