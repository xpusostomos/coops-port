(module coops-port
	(
	 <port> <input-port> <output-port> <binary-port>
     <binary-input-port> <binary-output-port>
     close closed? get-position set-position! 
     available flush! read! write! read-byte! write-byte! peek-byte!
     sync! ;; New generic for fsync/fdatasync

     ;; --- Constants & Enumerations ---
     whence/beginning whence/current whence/end
     
     ;; --- Unix File Flags (SRFI-170 style) ---
     open/read open/write open/read-write
     open/create open/exclusive open/truncate open/append

     ;; --- Bytevector Port Implementation ---
     <bytevector-port> <bytevector-input-port> <bytevector-output-port>
     <bytevector-dynamic-output-port>
     make-bytevector-input-port
     make-bytevector-output-port
     make-bytevector-dynamic-output-port
     reset-input-data!
     truncate!
     get-output-u8vector
     get-data-length
     get-buffer-capacity
     set-buffer-capacity!

     ;; --- Raw File Port Implementation ---
     <file-port> <file-input-port> <file-output-port>
     make-file-input-port
     make-file-output-port

     ;; --- Buffered Decorators ---
     <buffered-port> <buffered-input-port> <buffered-output-port>
     make-buffered-input-port
     make-buffered-output-port
     fill!
	 peek!
	 chicken-port->coops-port
	 coops-port->chicken-port
)
  
  (import scheme coops srfi-4 (chicken base) (chicken port) (chicken memory) (chicken bitwise) (chicken foreign)
		  (chicken locative) (chicken gc) (chicken blob))

(define-class <port> ()
  ((chicken-port initform: #f)
   (buffer initform: (make-u8vector 1))))

(define-generic (close port)
  @("Close port"
	(port "port")
	(@to "undefined")))

(define-method (close (port <port>))
  (void))

(define-generic (closed? port)
  @("Is the port closed?"
	(port "port")
	(@to "boolean")))

(define-method (closed? (port <port>))
  #f)

(define-generic (get-position port)
  @("Returns an object representing the location in the stream.
If it's a number, it means bytes from the beginning. If it's some other object
it is something opaque you can pass to set-position! If the port doesn't support
this feature, it returns #f"
	(port "port")
	(@to "number or object or #f")))

(define-method (get-position (port <port>))
  #f)

(define whence/beginning (foreign-value "SEEK_SET" int))
(define whence/current (foreign-value "SEEK_CUR" int))
(define whence/end (foreign-value "SEEK_END" int))

(define-generic (set-position! port position)
  @("Reset the position in the stream"
	(port "port")
	(position "object returned from get-position")
	(whence "If position is an integer, seek relative to that location.
If position is not an integer, whence is ignored and the stream is restored state represented by the opaque object.")
	(@to "#f if unsupported by this stream")))

(define-method (set-position! (port <port>) position #!optional (whence whence/beginning))
  #f)

(define-class <input-port> (<port>))

(define-generic (available <port>)
  @("Is data available?"
	(port "port")
	(@to "exact number")))

(define-method (available (port <port>))
  0)

(define-class <output-port> (<port>))

(define-generic (flush! <port>))

(define-method (flush! (port <port>))
  (void))

(define-class <textual-port> (<port>))

(define-class <binary-port> (<port>))

(define-class <binary-input-port> (<binary-port> <input-port>))

(define-generic (read! port bytevector))

(define-method (read! (port <port>) (dest-str string) start count)
  ;; Zero-copy: Alias the string's memory as a u8vector
  (let ((alias (blob->u8vector (string->blob dest-str))))
    (read! port alias start count)))

(define-generic (read-byte! port)
  @("Read one octet from the port"
	(port "port")
	(@to "octet or #eof")))


(define-method (read-byte! (port <binary-input-port>))
  (let* ((bv (make-u8vector 1 0)))
    (if (= (read! port bv 0 1) 1)
        (u8vector-ref bv 0)
        #!eof)))

(define-generic (peek-byte! port)
  @("Returns the next byte from the port without advancing the offset"
    (port "port")
    (@to "octet or #!eof")))

(define-class <binary-output-port> (<binary-port> <output-port>))

(define-generic (write! port bytevector)
  ;#!optional (start 0) (count #f))
  @("It is an error if the following conditions on the arguments are not met: start and count are non-negative exact integers, and bytevector is a bytevector whose length is at least start + count.

The write! procedure writes up to count bytes from bytevector starting at index start to the byte sink. The write! procedure returns the number of bytes that it wrote, as an exact integer."
	(port "port")
	(bytevector "location to store")
	(start "offset into bytevector")
	(count "max number of bytes to read")
	(@to "exact integer"))
  )

(define-generic (write-byte! port byte)
  @("writes the byte to the output stream"
	(@to "undefined")))

(define-method (write-byte! (port <binary-output-port>) byte)
  (let* ((bv (make-u8vector 1 0)))
    (u8vector-set! bv 0 byte)
    (write! port bv 0 1)
    (void)))


(define-class <bytevector-port> (<binary-port>)
  ((start initform: 0)
   (offset initform: 0)
   (size initform: 0)
   (limit initform: 0)
   (data)))

(define-class <bytevector-input-port> (<binary-input-port> <bytevector-port>))

(define (make-bytevector-input-port data #!optional (start 0) (count #f) (limit #f))
  (let* ((data-len (u8vector-length data))
         (actual-limit (or limit (- data-len start)))
         (actual-count (or count (- data-len start)))
         (size (+ start actual-count))
		 (nlimit (+ start actual-limit)))
    (make <bytevector-input-port> 
      'data  data
      'start start
      'offset start
      'size  size
      'limit nlimit)))

(define-method (reset-input-data! (port <bytevector-input-port>) offset size)
  (let ((start (slot-value port 'start)))
	(set! (slot-value port 'offset) (+ start offset))
	(set! (slot-value port 'size) (+ start size))
  ))

(define-method (read! (port <bytevector-input-port>) target #!optional (start 0) (count #f))
  (let* ((io-len (cond ((u8vector? target) (u8vector-length target))
						   ((integer? target) target)
                           ((string? target)   (string-length target))
                           ((blob? target)     (blob-size target)) ;; Blobs use 'size'
                           (else (error "read! target must be u8vector, string, or blob" target))))
		 (io-buf (cond ((u8vector? target) target)
					   ((integer? target) (make-u8vector target 0))
                       (else (make-locative target))))
		 (data (slot-value port 'data))
         (offset (slot-value port 'offset))
         (avail (available port))
         ;; How much space is actually in the target?
         (dest-cap (- io-len start))
         ;; How much did they ask for?
         (requested (or count dest-cap))
         ;; The "Clever" Count: Smallest of Request, Source Avail, and Dest Room
         (to-copy (max 0 (min requested avail dest-cap))))
	(move-memory! data io-buf to-copy offset start)
      (set! (slot-value port 'offset) (+ offset to-copy))
    to-copy))


(define-method (read-byte! (port <bytevector-input-port>))
  (let ((offset (slot-value port 'offset)))
	(if (< offset (slot-value port 'size))
		(begin
		  (set! (slot-value port 'offset) (+ offset 1))
		  (u8vector-ref (slot-value port 'data) offset))
        #!eof)))

(define-method (peek-byte! (port <bytevector-input-port>))
  (let ((offset (slot-value port 'offset))
        (size   (slot-value port 'size)))
    (if (< offset size)
        (u8vector-ref (slot-value port 'data) offset)
        #!eof)))

(define-method (available (port <bytevector-port>))
  (max 0 (- (slot-value port 'size) (slot-value port 'offset))))

(define-method (get-position (port <bytevector-port>))
  (- (slot-value port 'offset) (slot-value port 'start)))


(define-method (peek! (port <bytevector-input-port>) target #!optional (start 0) (count #f))
  (let* ((io-len (cond ((u8vector? target) (u8vector-length target))
						   ((integer? target) target)
                           ((string? target)   (string-length target))
                           ((blob? target)     (blob-size target)) ;; Blobs use 'size'
                           (else (error "read! target must be u8vector, string, or blob" target))))
		 (io-buf (cond ((u8vector? target) target)
					   ((integer? target) (make-u8vector target 0))
                       (else (make-locative target))))
		 (data      (slot-value port 'data))
         (offset    (slot-value port 'offset))
         (limit     (slot-value port 'limit)) ;; The logical "wall"
         (requested (or count (- io-len start)))
         ;; Use available, which is already calculated as (- size offset)
         (to-copy   (min requested (available port))))
    
    (move-memory! data io-buf to-copy offset start)
	to-copy))

(define-method (set-position! (port <bytevector-port>) position #!optional (whence whence/beginning))
  (let* ((s (slot-value port 'start))
         (l (slot-value port 'limit))
         (h (slot-value port 'size))
         ;; Calculate the new ABSOLUTE position
         (target-pos (cond
                       ((eq? whence whence/beginning) (+ s position))
                       ((eq? whence whence/current)   (+ (slot-value port 'offset) position))
                       ((eq? whence whence/end)       (+ h position))
                       (else (slot-value port 'offset)))))
    ;; Clamp absolute pos within [start, limit]
    (set! (slot-value port 'offset) (max s (min l target-pos)))
    ;; Return the relative position to the user
    (get-position port)))


(define-class <bytevector-output-port> (<binary-output-port> <bytevector-port>)
  )

(define (make-bytevector-output-port data #!optional (start 0) (count #f))
  (let* ((data-len (u8vector-length data))
         (actual-count (or count (- data-len start)))
         (end (+ start actual-count)))
    (make <bytevector-output-port>
          'data   data
          'start  start
          'offset start
          'size   start  ;; Initially 0 bytes written
          'limit  end)))

(define-method (write! (port <bytevector-output-port>) target #!optional (start 0) (count #f))
  (let* ((io-len (cond ((u8vector? target) (u8vector-length target))
						   ((integer? target) target)
                           ((string? target)   (string-length target))
                           ((blob? target)     (blob-size target)) ;; Blobs use 'size'
                           (else (error "read! target must be u8vector, string, or blob" target))))
		 (io-buf (cond ((u8vector? target) target)
					   ((integer? target) (make-u8vector target 0))
                       (else (make-locative target))))
		 (requested (or count (- io-len start)))
         (p (slot-value port 'offset)))
    
    ;; 1. Prepare the sandbox (Dynamic: grows 'limit'; Fixed: does nothing)
    (ensure-capacity! port (+ p requested))
    
    (let* ((limit (slot-value port 'limit))
           (to-write (min requested (max 0 (- limit p)))))
      
      (move-memory! io-buf (slot-value port 'data) to-write start p)
      
      ;; 2. Update cursor
      (set! (slot-value port 'offset) (+ p to-write))
      
      ;; 3. Update High-Water Mark (size) only if we extended the content
      (when (> (slot-value port 'offset) (slot-value port 'size))
        (set! (slot-value port 'size) (slot-value port 'offset)))
        
      to-write)))

(define-method (write-byte! (port <bytevector-output-port>) byte)
  (let ((offset (slot-value port 'offset)))
	(ensure-capacity! port (+ offset 1))
	(if (< offset (slot-value port 'limit))
		(begin
		  (u8vector-set! (slot-value port 'data) offset byte)
		  (set! (slot-value port 'offset) (+ offset 1))
		  (when (> (slot-value port 'offset) (slot-value port 'size))
			(set! (slot-value port 'size) (slot-value port 'offset)))))))


(define-generic (ensure-capacity! port req))

;; Fixed ports do nothing—they just let write! clamp to existing size
(define-method (ensure-capacity! (port <bytevector-output-port>) req)
  (void))

(define-generic (truncate! (port <bytevector-output-port>)))

(define-method (truncate! (port <bytevector-output-port>) length)
  (let* ((s (slot-value port 'start))
         (l (slot-value port 'limit))
         (new-hwm (+ s length))
		 (new-size (max s (min l new-hwm))))
    ;; Clamp high-water mark to [start, limit]
    (set! (slot-value port 'size) new-size)
    ;; Pull cursor back if it's now out of bounds
    (when (> (slot-value port 'offset) new-size)
      (set! (slot-value port 'offset) new-size))
    ;; Return relative size
    (- new-size s)))


(define-class <bytevector-dynamic-output-port> (<bytevector-output-port>)
  )

(define initial-buffer-capacity 32)

(define (make-bytevector-dynamic-output-port #!optional (initial-cap initial-buffer-capacity))
  (let ((data (make-u8vector initial-cap 0)))
    (make <bytevector-dynamic-output-port>
          'data   data
          'start  0
          'offset 0
          'size   0
          'limit  initial-cap)))

(define-method (initialize-instance (port <bytevector-dynamic-output-port>))
  (call-next-method)
  ;; Ensure we have at least a tiny buffer to start with for the doubling math
  (unless (slot-value port 'data)
    (set! (slot-value port 'data) (make-u8vector initial-buffer-capacity 0))))

(define-generic (calculate-growth port requested-size))

(define-method (calculate-growth (port <bytevector-dynamic-output-port>) req)
  ;; Default strategy: double the current capacity
  (max req (* (u8vector-length (slot-value port 'data)) 2)))

(define-method (ensure-capacity! (port <bytevector-dynamic-output-port>) req)
  (let* ((data (slot-value port 'data))
         (current-cap (u8vector-length data)))
    
    (when (> req current-cap)
      (let* ((new-cap (calculate-growth port req))
             (new-data (make-u8vector new-cap 0)))
        ;; Copy all existing data up to the high-water mark
        (move-memory! data new-data (slot-value port 'size) 0 0)
        (set! (slot-value port 'data) new-data)))
    
    ;; Push the "Hard Wall" (limit) to match the request or new capacity
    (when (> req (slot-value port 'limit))
      (set! (slot-value port 'limit) (u8vector-length (slot-value port 'data))))))

(define-method (truncate! (port <bytevector-dynamic-output-port>) new-size)
  ;; 1. Use the factored-out logic to ensure physical memory exists
  (ensure-capacity! port new-size)
  
  ;; 2. Capacity is already handled by ensure-capacity! 
  ;; but we call next-method to update the 'size' and 'offset' slots
  (call-next-method))

(define-generic (set-buffer-capacity! port))


(define-method (set-buffer-capacity! (port <bytevector-dynamic-output-port>) #!optional (size initial-buffer-capacity))
  (let* ((hwm (slot-value port 'size))
         (current-limit (slot-value port 'limit))
         ;; The floor is the high-water mark; we cannot shrink past our data.
         (target-cap (max hwm size)))
    
    (unless (= target-cap current-limit)
      (let ((new-data (make-u8vector target-cap 0))
            (old-data (slot-value port 'data)))
        
        ;; Move all valid data (size) into the new allocation
        (move-memory! old-data new-data hwm 0 0)
        
        (set! (slot-value port 'data) new-data)
        (set! (slot-value port 'limit) target-cap)))
    
    ;; Return the relative limit
    (slot-value port 'limit)))

(define-generic (get-buffer-capacity port))

(define-method (get-buffer-capacity (port <bytevector-port>))
  (- (slot-value port 'limit) (slot-value port 'start)))

(define-generic (get-output-u8vector port))

(define-method (get-output-u8vector (port <bytevector-port>))
  (let* ((data (slot-value port 'data))
         (s (slot-value port 'start))
         (h (slot-value port 'size)))
    ;; Return a copy of the segment from start to the high-water mark
    (subu8vector data s h)))

(define-method (get-data-length (port <bytevector-port>))
  (- (slot-value port 'size) (slot-value port 'start)))

(define-class <buffered-port> (<output-port>)
  ((source/sink)          ;; Any port-like object that implements write! and flush!
   (buffer)        ;; The <bytevector-fixed-output-port> logic manager
   (raw-buffer)))  ;; The physical u8vector backing the buffer

(define-method (get-data-length (port <buffered-port>))
  (let ((source (slot-value port 'source/sink)))
    (get-data-length source)))

(define-class <buffered-output-port> (<buffered-port> <binary-output-port> )
  )

(define (make-buffered-output-port source/sink #!optional (size 4096))
  (let ((raw (make-u8vector size 0)))
    (make <buffered-output-port>
          'source/sink source/sink
          'buffer (make-bytevector-output-port raw)
          'raw-buffer raw)))

(define-method (write! (port <buffered-output-port>) bytevector #!optional (start 0) (count #f))
  (let* ((requested   (or count (- (u8vector-length bytevector) start)))
         (buf         (slot-value port 'buffer))
         (source/sink (slot-value port 'source/sink))
         (capacity    (get-buffer-capacity buf))
         (p           (get-position buf))
         (free-space  (- capacity p)))
	
    (cond
     ;; 1. Fits entirely in the current buffer
     ((<= requested free-space)
      (write! buf bytevector start requested))
	 
     ;; 2. Larger than the total buffer capacity: BYPASS
     ((> requested capacity)
      (flush! port)
      (write! source/sink bytevector start requested))
	 
     ;; 3. Fits in the buffer, but not in the *current* free space: SPLIT/REFILL
     (else
      ;; Fill up the remaining free space and dump it
      (write! buf bytevector start free-space)
      (flush! port)
      ;; Put the leftover in the now-empty buffer
      (write! buf bytevector (+ start free-space) (- requested free-space))))))


(define-method (flush! (port <buffered-output-port>))
  (let ((buf  (slot-value port 'buffer))
        (source/sink (slot-value port 'source/sink))
        (raw  (slot-value port 'raw-buffer)))
    (write! source/sink raw 0 (get-data-length buf))
    (truncate! buf 0)
    (flush! source/sink)))

(define-method (write-byte! (port <buffered-output-port>) byte)
  (let ((buf (slot-value port 'buffer)))
    ;; If the internal buffer is full, dump it to the sink
    (when (zero? (- (get-buffer-capacity buf) (get-position buf)))
      (flush! port))
    ;; Use the fast bytevector-output-port write-byte
    (write-byte! buf byte)))

(define-class <buffered-input-port> (<buffered-port> <binary-input-port> )
  )


(define (make-buffered-input-port source/sink #!optional (size 4096))
  (let ((raw (make-u8vector size 0)))
    (make <buffered-input-port>
          'source/sink source/sink
          'buffer (make-bytevector-input-port raw 0 0 size) ;; big buffer, but nothing there yet
          'raw-buffer raw)))

(define-method (read! (port <buffered-input-port>) bytevector #!optional (start 0) (count #f))
  (let* ((requested (or count (- (u8vector-length bytevector) start)))
         (buf       (slot-value port 'buffer))
         (cap       (u8vector-length (slot-value port 'raw-buffer)))
         (drained   (read! buf bytevector start requested))
         (remaining (- requested drained)))
    
    (+ drained
       (cond ((zero? remaining) 0)
			 ((> remaining cap)
              ;; --- BYPASS BRANCH ---
              ;; We move the source independently of the buffer. 
              ;; We MUST nuke the buffer to prevent "Stale Window" hits.
              (reset-input-data! buf 0 0)
              (read! (slot-value port 'source/sink) bytevector (+ start drained) remaining))
             
             ;; --- REFILL BRANCH ---
             (else (fill! port) ;; fill! handles its own reset-input-data!
				   ;; NO MANUAL RESET HERE!
				   (let ((to-read (min remaining (available buf))))
					 (read! buf bytevector (+ start drained) to-read)))))))

(define-method (get-position (port <buffered-output-port>))
  (let* ((source (slot-value port 'source/sink))
         (buf    (slot-value port 'buffer))
         (source-pos (get-position source)))
    (if (not source-pos)
        #f
        ;; User is ahead of the source by the amount currently sitting in the buffer
        (+ source-pos (get-data-length buf)))))


(define-method (set-position! (port <buffered-input-port>) position #!optional (whence whence/beginning))
  (let* ((buf    (slot-value port 'buffer))
         (source (slot-value port 'source/sink)))
    
    (cond
     ;; --- Case A: Relative Seek (Socket-friendly / 0 Syscalls) ---
     ;; We check the delta against what's actually in the manager right now.
     ((eq? whence whence/current)
      (let ((avail (available buf))
            (used  (get-position buf))) 
        (if (and (>= position (- used)) (<= position avail))
            ;; CACHE HIT: We just slide the manager's offset.
            (set-position! buf position whence/current)
            ;; CACHE MISS: We finally admit we need the ground truth.
            (let* ((sp (get-position source))
                   (logical-now (- sp avail)))
              (reset-input-data! buf 0 0)
              (set-position! source (+ logical-now position) whence/beginning)))))

     ;; --- Case B: Absolute Seek (Requires Window Math) ---
     (else
      (let* ((target-pos (if (eq? whence whence/beginning) 
                             position 
                             (+ (get-position source 0 whence/end) position)))
             ;; We calculate where the buffer window starts in the file
             ;; using the invariant: SourcePos - DataLength
             (sp (get-position source))
             (dl (get-data-length buf))
             (window-start (- sp dl)))
        
        (if (and (>= target-pos window-start) (<= target-pos sp))
            ;; CACHE HIT: Move manager relative to the calculated window start
            (set-position! buf (- target-pos window-start) whence/beginning)
            ;; CACHE MISS: Nuke and move hardware
            (begin
              (reset-input-data! buf 0 0)
              (set-position! source target-pos whence/beginning))))))))

(define-method (c (port <buffered-input-port>) n)
  (let ((raw      (slot-value port 'raw-buffer)))
	(u8vector-ref raw n)))

(define (print-u8vector vec)
  (for-each (lambda (byte)
              (display byte)
              (display " "))
            (u8vector->list vec))
  (newline))


(define-method (fill! (port <buffered-input-port>))
  (let* ((source   (slot-value port 'source/sink))
         (buf-mgr  (slot-value port 'buffer))
         (raw      (slot-value port 'raw-buffer))
         (capacity (get-buffer-capacity buf-mgr)))
	(when (zero? (available buf-mgr))
        (reset-input-data! buf-mgr 0 0))

    ;; 2. Part 2: Reuse the same variable names for the actual read logic.
    (let* ((pos     (get-position buf-mgr))
           (cur-len (get-data-length buf-mgr))
           (room    (- capacity cur-len)))
      (if (> room 0)
          (let ((sz (read! source raw cur-len room)))
            (reset-input-data! buf-mgr pos (+ cur-len sz))
            sz)
          0))))


(define-method (peek! (port <buffered-input-port>) target #!optional (start 0) (count #f))
  (let* ((io-len (cond ((u8vector? target) (u8vector-length target))
                           ((string? target)   (string-length target))
                           ((blob? target)     (blob-size target)) ;; Blobs use 'size'
                           (else (error "read! target must be u8vector, string, or blob" target))))
		 (buf-mgr    (slot-value port 'buffer))
         (raw-buffer (slot-value port 'raw-buffer))
         (capacity   (get-buffer-capacity buf-mgr))
         (requested  (or count (- io-len start))))

    (when (> requested capacity)
      (error "peek! request exceeds buffer capacity" requested capacity))

    (let ((avail (available buf-mgr)))
      ;; 1. If we don't have enough, shuffle and refill.
      (when (< avail requested)
        (let ((rel-off (get-position buf-mgr)))
          (move-memory! raw-buffer raw-buffer avail rel-off 0)
          (reset-input-data! buf-mgr 0 avail)
          (fill! port)))

      ;; 2. CACHE HIT: The data is now hopefully in the buffer's window
      ;; Just delegate the actual copy-out to the buffer's peek! method.
      (peek! buf-mgr target start requested))))
  
(define-method (read-byte! (port <buffered-input-port>))
  (let* ((buffer (slot-value port 'buffer))
		(c (read-byte! buffer)))
	(if (eof-object? c)
		(begin
		  (fill! port)
		  (read-byte! buffer))
		c)))

(define-method (peek-byte! (port <buffered-input-port>))
  (let ((buf-mgr (slot-value port 'buffer)))
    (if (> (available buf-mgr) 0)
        ;; Fast path: We have data, just grab it from the manager
        (peek-byte! buf-mgr)
        ;; Slow path: Buffer is empty. Let the existing peek! 
        ;; handle the complex refill/shuffle logic.
        (let ((bv (make-u8vector 1 0)))
          (if (= (peek! port bv 0 1) 1)
              (u8vector-ref bv 0)
              #!eof)))))

(define-method (available (port <buffered-input-port>))
  (+ (available (slot-value port 'buffer))
     (available (slot-value port 'source/sink))))


(foreign-declare "#include <fcntl.h>")

(define %posix-read% (foreign-lambda int "read" int c-pointer int))
(define %posix-lseek% (foreign-lambda int "lseek" int int int))
(define %posix-close% (foreign-lambda int "close" int))
(define %posix-open%  (foreign-lambda int "open" c-string int int))
(define %posix-write% (foreign-lambda int "write" int c-pointer int))
(define %posix-fsync% (foreign-lambda int "fsync" int))
(define %posix-fdatasync% (foreign-lambda int "fdatasync" int))

(define open/read        (foreign-value "O_RDONLY" int))
(define open/write       (foreign-value "O_WRONLY" int))
(define open/read-write  (foreign-value "O_RDWR"   int))
(define open/create @("Create file if it doesn't exist") (foreign-value "O_CREAT"  int))
(define open/exclusive @("Fails if the file exists") (foreign-value "O_EXCL"   int))
(define open/truncate @("Truncates the file") (foreign-value "O_TRUNC"  int))
(define open/append @("Always appends to the file") (foreign-value "O_APPEND" int))


(define-class <file-port> (<binary-port>)
  ((fd initform: -1)))

(define-method (initialize-instance (port <file-port>))
  (call-next-method)
  ;; Register the generic 'close' for this specific instance
  (set-finalizer! port close))

(define-method (closed? (port <file-port>))
  (< (slot-value port 'fd) 0))

(define-method (close (port <file-port>))
  (let ((fd (slot-value port 'fd)))
    (when (>= fd 0)
      (%posix-close% fd)
      (set! (slot-value port 'fd) -1)
	  (set-finalizer! port (lambda (p) (void))))))

(define-method (set-position! (port <file-port>) position #!optional (whence whence/beginning))
  (let* ((fd (slot-value port 'fd))
         (new-pos (%posix-lseek% fd position whence)))
    (if (negative? new-pos) #f new-pos)))

(define-method (get-position (port <file-port>))
  (let* ((fd (slot-value port 'fd))
         (new-pos (%posix-lseek% fd 0 whence/current)))
    (if (negative? new-pos) #f new-pos)))

(define-generic (sync! port))

(define-method (sync! (port <file-port>) #!optional (data-only? #f))
  (let ((fd (slot-value port 'fd)))
    (if (>= fd 0)
        (let* ((sync-fn (if data-only? %posix-fdatasync% %posix-fsync%))
               (result (sync-fn fd)))
          (if (negative? result)
              (error "sync! failed" fd)
              result))
        (void)))) ;; Silently succeed if already closed

(define-class <file-input-port> (<binary-input-port> <file-port>)
  )

(define (make-file-input-port path #!optional (flags open/read) (mode #o644))
  (let ((fd (%posix-open% path flags mode)))
    (if (negative? fd)
        (begin 
          (error "Could not open file raw" path))
        (make <file-input-port> 'fd fd))))

(define-method (read! (port <file-input-port>) bytevector #!optional (start 0) (count #f))
  (let* ((fd (slot-value port 'fd))
         (requested (or count (- (u8vector-length bytevector) start)))
         ;; We must use a locative so the FFI 'c-pointer' type gets a raw address
         (ptr (make-locative bytevector start))
         (result (%posix-read% fd ptr requested)))
    (if (negative? result)
        (error "file-read error" result)
        result)))


(define-method (get-position (port <buffered-input-port>))
  (let* ((source (slot-value port 'source/sink))
         (buf    (slot-value port 'buffer))
         (source-pos (get-position source)))
    (if (not source-pos)
        #f
        ;; The source is ahead of the user by the number of unread bytes in the buffer
        (- source-pos (available buf)))))


(foreign-declare "#include <sys/stat.h>"
)

(define %posix-fstat-size% 
  (foreign-lambda* int ((int fd))
    "struct stat buf;
     int size = -1;
     if (fstat(fd, &buf) == 0) size = buf.st_size;
     C_return(size);"))

(define %posix-socket-available%
  (foreign-lambda* int ((int fd))
    "int count = -1;
     #ifdef FIONREAD
     if (ioctl(fd, FIONREAD, &count) == -1) count = -1;
     #endif
     C_return(count);"))

(define-method (get-data-length (port <file-port>))
  (let ((size (%posix-fstat-size% (slot-value port 'fd))))
    (if (negative? size) 0 size)))

(define-method (available (port <file-input-port>))
  (let ((total (get-data-length port))
        (curr  (get-position port)))
    (if (and total curr)
        (max 0 (- total curr))
        0)))


(define-class <file-output-port> (<binary-output-port> <file-port>)
  )

(define (make-file-output-port path #!optional (flags (bitwise-ior open/write open/create open/truncate)) (mode #o644))
  (let ((fd (%posix-open% path flags mode)))
    (if (negative? fd)
        (error "Could not open file raw" path)
        (make <file-output-port> 'fd fd))))


(define-method (write! (port <file-output-port>) bytevector #!optional (start 0) (count #f))
  (let* ((fd (slot-value port 'fd))
         (requested (or count (- (u8vector-length bytevector) start)))
         ;; Same here: c-pointer requires a locative or raw pointer
         (ptr (make-locative bytevector start))
         (result (%posix-write% fd ptr requested)))
    (if (negative? result)
        (error "file-write error" result)
        result)))

(define (copy-port! in out #!optional (buf-size 65536))
  (let ((buffer (make-u8vector buf-size 0)))
    (let loop ()
      (let ((n (read! in buffer)))
        (if (zero? n)
            (void) ;; Done
            (begin
              (write! out buffer 0 n)
              (loop)))))))


(define (make-coops-wrapper-port coops-obj)
  (let* ((class
          (vector 
           ;; 0: read-char (Must return #\char or #!eof)
           (lambda (p) 
             (let ((b (read-byte! coops-obj)))
               (if (eof-object? b) b (integer->char b))))
           
           ;; 1: peek-char (Must return #\char or #!eof)
           (lambda (p) 
             (let ((b (peek-byte! coops-obj)))
               (if (eof-object? b) b (integer->char b))))
           
           ;; 2: write-char (Standard char->integer)
           (lambda (p c) (write-byte! coops-obj (char->integer c))) 
           
           ;; 3: write-string (Standard bytes-out)
           (lambda (p s) (write! coops-obj s))      
           
           ;; 4: close
           (lambda (p d) (close coops-obj))         
           
           ;; 5: flush-output
           (lambda (p) (flush! coops-obj))          
           
           ;; 6: char-ready?
           (lambda (p) (> (available coops-obj) 0)) 
           
           ;; 7: read-string! (The optimized block-read hook)
           ;; CHICKEN passes: (port dest-string count)
           (lambda (port count target offset) 
             (read!
			  coops-obj
			  target
			  offset
			  count))
           
           ;; 8: read-line (We let CHICKEN use its default logic)
           #f))                                     
         
         (type (if (subclass? (class-of coops-obj) <input-port>) 1 2))
         (c-port (##sys#make-port type class "(coops-port)" 'custom))
         (data (vector #f coops-obj)))
    
    (##sys#set-port-data! c-port data)
    (set! (slot-value coops-obj 'chicken-port) c-port)
    c-port))

(define (chicken-port->coops-port c-port)
  (if (port? c-port)
      (let ((data (##sys#port-data c-port))) 
        (if (and (vector? data) (>= (vector-length data) 2))
            (##sys#slot data 1) ;; Extract from the 2nd slot of the data vector
            #f))
      #f))

(define (coops-port->chicken-port coops-port)
  (let ((h (slot-value coops-port 'chicken-port)))
    ;; If the slot is unbound or #f, create the wrapper now
    (if (port? h) 
        h
        (make-coops-wrapper-port coops-port))))


)
