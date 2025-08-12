;; Simple Wi-Fi Password Share Contract
;; Secure internet access sharing with usage limits and cost splitting

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-payment (err u103))
(define-constant err-usage-exceeded (err u104))

;; Data Variables
(define-data-var total-shares-created uint u0)

;; Data Maps
(define-map wifi-shares
  { share-id: uint }
  {
    owner: principal,
    password-hash: (buff 32),
    cost-per-gb: uint,
    max-usage-gb: uint,
    created-at: uint,
    active: bool
  }
)

(define-map user-sessions
  { share-id: uint, user: principal }
  {
    total-paid: uint,
    usage-gb: uint,
    last-activity: uint
  }
)

(define-map share-earnings
  { share-id: uint }
  { total-earned: uint }
)

;; Public Functions

;; Create a new Wi-Fi share
(define-public (create-wifi-share (password-hash (buff 32)) (cost-per-gb uint) (max-usage-gb uint))
  (let ((share-id (+ (var-get total-shares-created) u1)))
    (map-set wifi-shares
      { share-id: share-id }
      {
        owner: tx-sender,
        password-hash: password-hash,
        cost-per-gb: cost-per-gb,
        max-usage-gb: max-usage-gb,
        created-at: stacks-block-height,
        active: true
      }
    )
    (map-set share-earnings { share-id: share-id } { total-earned: u0 })
    (var-set total-shares-created share-id)
    (ok share-id)
  )
)

;; Purchase access to a Wi-Fi share
(define-public (purchase-access (share-id uint) (payment uint))
  (let (
    (share-info (unwrap! (map-get? wifi-shares { share-id: share-id }) err-not-found))
    (current-session (default-to
      { total-paid: u0, usage-gb: u0, last-activity: u0 }
      (map-get? user-sessions { share-id: share-id, user: tx-sender })
    ))
  )
    (asserts! (get active share-info) err-not-found)
    (asserts! (>= payment (get cost-per-gb share-info)) err-insufficient-payment)

    (try! (stx-transfer? payment tx-sender (get owner share-info)))

    (map-set user-sessions
      { share-id: share-id, user: tx-sender }
      {
        total-paid: (+ (get total-paid current-session) payment),
        usage-gb: (get usage-gb current-session),
        last-activity: stacks-block-height
      }
    )

    (map-set share-earnings
      { share-id: share-id }
      { total-earned: (+ (default-to u0 (get total-earned (map-get? share-earnings { share-id: share-id }))) payment) }
    )

    (ok true)
  )
)

;; Record usage (called by owner)
(define-public (record-usage (share-id uint) (user principal) (usage-gb uint))
  (let (
    (share-info (unwrap! (map-get? wifi-shares { share-id: share-id }) err-not-found))
    (current-session (unwrap! (map-get? user-sessions { share-id: share-id, user: user }) err-not-found))
  )
    (asserts! (is-eq tx-sender (get owner share-info)) err-unauthorized)
    (asserts! (<= (+ (get usage-gb current-session) usage-gb) (get max-usage-gb share-info)) err-usage-exceeded)

    (map-set user-sessions
      { share-id: share-id, user: user }
      {
        total-paid: (get total-paid current-session),
        usage-gb: (+ (get usage-gb current-session) usage-gb),
        last-activity: stacks-block-height
      }
    )

    (ok true)
  )
)

;; Toggle share active status
(define-public (toggle-share-status (share-id uint))
  (let ((share-info (unwrap! (map-get? wifi-shares { share-id: share-id }) err-not-found)))
    (asserts! (is-eq tx-sender (get owner share-info)) err-unauthorized)

    (map-set wifi-shares
      { share-id: share-id }
      (merge share-info { active: (not (get active share-info)) })
    )

    (ok (not (get active share-info)))
  )
)

;; Read-only Functions

(define-read-only (get-wifi-share (share-id uint))
  (map-get? wifi-shares { share-id: share-id })
)

(define-read-only (get-user-session (share-id uint) (user principal))
  (map-get? user-sessions { share-id: share-id, user: user })
)

(define-read-only (get-share-earnings (share-id uint))
  (map-get? share-earnings { share-id: share-id })
)

(define-read-only (get-password-hash (share-id uint))
  (match (map-get? wifi-shares { share-id: share-id })
    share-info (some (get password-hash share-info))
    none
  )
)

(define-read-only (can-access (share-id uint) (user principal))
  (match (map-get? user-sessions { share-id: share-id, user: user })
    session (> (get total-paid session) u0)
    false
  )
)

(define-read-only (get-remaining-usage (share-id uint) (user principal))
  (match (map-get? wifi-shares { share-id: share-id })
    share-info
      (match (map-get? user-sessions { share-id: share-id, user: user })
        session (- (get max-usage-gb share-info) (get usage-gb session))
        (get max-usage-gb share-info)
      )
    u0
  )
)
