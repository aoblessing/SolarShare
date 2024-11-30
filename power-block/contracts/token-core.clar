;; title: token-core
;; version: 1.0
;; description: Core NFT contract for SolarShare power tokens

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-INVALID-PARAMETERS (err u103))

;; Data Variables
(define-data-var token-id-nonce uint u0)
(define-data-var total-installations uint u0)

;; NFT Definition
(define-non-fungible-token power-share uint)

;; Data Maps
(define-map token-metadata
    uint  ;; token-id
    {
        installation-id: uint,
        power-capacity: uint,        ;; in watts
        minted-at: uint,            ;; block height
        last-transfer: uint         ;; block height
    }
)

(define-map installation-details
    uint  ;; installation-id
    {
        location: (string-ascii 50),
        total-capacity: uint,        ;; in watts
        active-tokens: uint,         ;; number of active tokens
        created-at: uint,           ;; block height
        verified: bool              ;; installation verification status
    }
)

;; Read-Only Functions

;; Get token metadata
(define-read-only (get-token-info (token-id uint))
    (ok (map-get? token-metadata token-id))
)

;; Get installation details
(define-read-only (get-installation-info (installation-id uint))
    (ok (map-get? installation-details installation-id))
)

;; Get token owner
(define-read-only (get-token-owner (token-id uint))
    (ok (nft-get-owner? power-share token-id))
)

;; Public Functions

;; Register new solar installation
(define-public (register-installation 
        (location (string-ascii 50))
        (total-capacity uint))
    (let
        ((installation-id (+ (var-get total-installations) u1)))
        (try! (assert-is-owner))
        (map-set installation-details installation-id
            {
                location: location,
                total-capacity: total-capacity,
                active-tokens: u0,
                created-at: block-height,
                verified: false
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
        
        ;; Verify installation exists and has capacity
        (asserts! (<= (+ power-capacity 
                        (* (get active-tokens installation) power-capacity))
                     (get total-capacity installation))
            ERR-INVALID-PARAMETERS)
        
        ;; Mint token
        (try! (nft-mint? power-share token-id recipient))
        
        ;; Update metadata
        (map-set token-metadata token-id
            {
                installation-id: installation-id,
                power-capacity: power-capacity,
                minted-at: block-height,
                last-transfer: block-height
            })
        
        ;; Update installation active tokens
        (map-set installation-details installation-id
            (merge installation 
                {active-tokens: (+ (get active-tokens installation) u1)}))
        
        (var-set token-id-nonce token-id)
        (ok token-id)))

;; Transfer token
(define-public (transfer-token (token-id uint) (recipient principal))
    (let ((owner (unwrap! (nft-get-owner? power-share token-id) ERR-NOT-FOUND))
          (token-data (unwrap! (map-get? token-metadata token-id) ERR-NOT-FOUND)))
        (asserts! (is-eq tx-sender owner) ERR-NOT-AUTHORIZED)
        (try! (nft-transfer? power-share token-id tx-sender recipient))
        
        ;; Update last transfer in metadata
        (map-set token-metadata token-id
            (merge token-data {last-transfer: block-height}))
        
        (ok true)))

;; Verify installation
(define-public (verify-installation (installation-id uint))
    (let ((installation (unwrap! (map-get? installation-details installation-id) ERR-NOT-FOUND)))
        (try! (assert-is-owner))
        (map-set installation-details installation-id
            (merge installation {verified: true}))
        (ok true)))

;; Private Functions

;; Check if caller is contract owner
(define-private (assert-is-owner)
    (if (is-eq tx-sender CONTRACT-OWNER)
        (ok true)
        ERR-NOT-AUTHORIZED))

;; Initialize contract
(begin
    (var-set token-id-nonce u0)
    (var-set total-installations u0))
