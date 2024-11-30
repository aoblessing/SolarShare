;; title: token-core
;; version: 1.1
;; description: Enhanced Core NFT contract for SolarShare power tokens

;; Constants for contract control
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-POWER-CAPACITY u100) ;; Minimum power capacity in watts
(define-constant MAX-POWER-CAPACITY u10000) ;; Maximum power capacity in watts

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-PARAMETERS (err u103))
(define-constant ERR-INSTALLATION-NOT-VERIFIED (err u104))
(define-constant ERR-INSUFFICIENT-CAPACITY (err u105))
(define-constant ERR-INSTALLATION-INACTIVE (err u106))
(define-constant ERR-POWER-CAPACITY-BOUNDS (err u107))
(define-constant ERR-TOKEN-LOCKED (err u108))
(define-constant ERR-INVALID-ADDRESS (err u109))

;; Data Variables
(define-data-var token-id-nonce uint u0)
(define-data-var total-installations uint u0)
(define-data-var contract-paused bool false)

;; NFT Definition
(define-non-fungible-token power-share uint)

;; Data Maps
(define-map token-metadata
    uint  ;; token-id
    {
        installation-id: uint,
        power-capacity: uint,        ;; in watts
        minted-at: uint,            ;; block height
        last-transfer: uint,        ;; block height
        locked-until: uint,         ;; block height for transfer lock
        power-used: uint           ;; tracking power usage
    }
)

(define-map installation-details
    uint  ;; installation-id
    {
        location: (string-ascii 50),
        total-capacity: uint,        ;; in watts
        active-tokens: uint,         ;; number of active tokens
        created-at: uint,           ;; block height
        verified: bool,             ;; installation verification status
        active: bool,               ;; installation operational status
        maintainer: principal,      ;; installation maintainer
        last-maintenance: uint      ;; last maintenance block height
    }
)

;; Track banned addresses
(define-map banned-addresses 
    principal 
    bool
)

;; Read-Only Functions

(define-read-only (get-token-info (token-id uint))
    (ok (map-get? token-metadata token-id))
)

(define-read-only (get-installation-info (installation-id uint))
    (ok (map-get? installation-details installation-id))
)

(define-read-only (get-token-owner (token-id uint))
    (ok (nft-get-owner? power-share token-id))
)

(define-read-only (is-banned (address principal))
    (default-to false (map-get? banned-addresses address))
)

(define-read-only (get-contract-status)
    (ok (var-get contract-paused))
)

;; Private Functions

(define-private (assert-is-owner)
    (if (is-eq tx-sender CONTRACT-OWNER)
        (ok true)
        ERR-NOT-AUTHORIZED))

(define-private (assert-not-banned (address principal))
    (if (is-banned address)
        ERR-NOT-AUTHORIZED
        (ok true)))

(define-private (assert-contract-not-paused)
    (if (var-get contract-paused)
        ERR-NOT-AUTHORIZED
        (ok true)))

(define-private (assert-valid-power-capacity (power-capacity uint))
    (if (and 
            (>= power-capacity MIN-POWER-CAPACITY)
            (<= power-capacity MAX-POWER-CAPACITY))
        (ok true)
        ERR-POWER-CAPACITY-BOUNDS))

(define-private (assert-installation-active (installation-id uint))
    (match (map-get? installation-details installation-id)
        installation (if (get active installation)
            (ok true)
            ERR-INSTALLATION-INACTIVE)
        ERR-NOT-FOUND))

;; Public Functions

;; Contract pause/unpause for emergencies
(define-public (set-contract-pause (pause bool))
    (begin
        (try! (assert-is-owner))
        (ok (var-set contract-paused pause))))

;; Ban/unban addresses
(define-public (set-address-ban (address principal) (banned bool))
    (begin
        (try! (assert-is-owner))
        (ok (map-set banned-addresses address banned))))

;; Register new solar installation
(define-public (register-installation 
        (location (string-ascii 50))
        (total-capacity uint)
        (maintainer principal))
    (let
        ((installation-id (+ (var-get total-installations) u1)))
        (try! (assert-contract-not-paused))
        (try! (assert-is-owner))
        (try! (assert-not-banned maintainer))
        (try! (assert-valid-power-capacity total-capacity))
        
        (map-set installation-details installation-id
            {
                location: location,
                total-capacity: total-capacity,
                active-tokens: u0,
                created-at: block-height,
                verified: false,
                active: true,
                maintainer: maintainer,
                last-maintenance: block-height
            })
        (var-set total-installations installation-id)
        (ok installation-id)))

;; Mint new power share token
(define-public (mint-power-share 
        (recipient principal)
        (installation-id uint)
        (power-capacity uint))
    (let
        ((token-id (+ (var-get token-id-nonce) u1))
         (installation (unwrap! (map-get? installation-details installation-id) ERR-NOT-FOUND)))
        
        ;; Comprehensive checks
        (try! (assert-contract-not-paused))
        (try! (assert-not-banned recipient))
        (try! (assert-valid-power-capacity power-capacity))
        (try! (assert-installation-active installation-id))
        
        ;; Verify installation is verified and has capacity
        (asserts! (get verified installation) ERR-INSTALLATION-NOT-VERIFIED)
        (asserts! (<= (+ power-capacity 
                        (* (get active-tokens installation) power-capacity))
                     (get total-capacity installation))
            ERR-INSUFFICIENT-CAPACITY)
        
        ;; Mint token
        (try! (nft-mint? power-share token-id recipient))
        
        ;; Update metadata
        (map-set token-metadata token-id
            {
                installation-id: installation-id,
                power-capacity: power-capacity,
                minted-at: block-height,
                last-transfer: block-height,
                locked-until: u0,
                power-used: u0
            })
        
        ;; Update installation active tokens
        (map-set installation-details installation-id
            (merge installation 
                {active-tokens: (+ (get active-tokens installation) u1)}))
        
        (var-set token-id-nonce token-id)
        (ok token-id)))

;; Transfer token with additional checks
(define-public (transfer-token (token-id uint) (recipient principal))
    (let 
        ((owner (unwrap! (nft-get-owner? power-share token-id) ERR-NOT-FOUND))
         (token-data (unwrap! (map-get? token-metadata token-id) ERR-NOT-FOUND)))
        
        ;; Comprehensive checks
        (try! (assert-contract-not-paused))
        (try! (assert-not-banned recipient))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (asserts! (>= block-height (get locked-until token-data)) ERR-TOKEN-LOCKED)
        
        ;; Perform transfer
        (try! (nft-transfer? power-share token-id tx-sender recipient))
        
        ;; Update metadata
        (map-set token-metadata token-id
            (merge token-data {last-transfer: block-height}))
        
        (ok true)))

;; Lock token transfers temporarily
(define-public (lock-token (token-id uint) (lock-period uint))
    (let ((owner (unwrap! (nft-get-owner? power-share token-id) ERR-NOT-FOUND))
          (token-data (unwrap! (map-get? token-metadata token-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (map-set token-metadata token-id
            (merge token-data 
                {locked-until: (+ block-height lock-period)}))
        (ok true)))

;; Update installation maintenance status
(define-public (update-maintenance (installation-id uint))
    (let ((installation (unwrap! (map-get? installation-details installation-id) ERR-NOT-FOUND)))
        (asserts! (or 
            (is-eq tx-sender (get maintainer installation))
            (is-eq tx-sender CONTRACT-OWNER)) 
            ERR-NOT-AUTHORIZED)
        
        (map-set installation-details installation-id
            (merge installation 
                {last-maintenance: block-height}))
        (ok true)))

;; Set installation active status
(define-public (set-installation-active (installation-id uint) (active bool))
    (let ((installation (unwrap! (map-get? installation-details installation-id) ERR-NOT-FOUND)))
        (try! (assert-is-owner))
        (map-set installation-details installation-id
            (merge installation {active: active}))
        (ok true)))

;; Initialize contract
(begin
    (var-set token-id-nonce u0)
    (var-set total-installations u0)
    (var-set contract-paused false))
