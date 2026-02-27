(import test 
        coops 
        srfi-4 
        (chicken base) 
        (chicken port) 
        (chicken memory)
        (chicken condition)
		(chicken bitwise)
        (only (chicken file) delete-file)
		coops-port)

(test-begin "Coop-Ports")

;; --- 1. Bytevector Input Ports ---
(test-group "1. Bytevector Input Ports"
  (let* ((source (u8vector 10 20 30 40 50 60))
         (in-port (make-bytevector-input-port source 2 3))) ;; Segment: [30, 40, 50]
    
    (test "Initial position is 0 (relative)" 0 (get-position in-port))
    (test "First byte read is correct (30)" 30 (read-byte! in-port))
    
    (test-group "1.1 Positioning & Seek"
      (set-position! in-port 0 whence/end)
      (test "Seek to end returns relative size (3)" 3 (get-position in-port))
      
      (set-position! in-port -1 whence/end)
      (test "Relative seek -1 from end is correct (50)" 50 (read-byte! in-port))
      
      (set-position! in-port 0 whence/beginning)
      (test "Seek back to beginning" 0 (get-position in-port)))
    
    (test "Available reporting" 3 (available in-port))))

;; --- 2. Output Ports (Fixed & Dynamic) ---
(test-group "2. Output Ports"
  
  (test-group "2.1 Fixed Sandbox"
    (let* ((dest (make-u8vector 10 0))
           (out (make-bytevector-output-port dest 5 3))) ;; Index 5,6,7
      (test "Write within capacity" 2 (write! out (u8vector 1 2)))
      (test "Short write on overflow" 1 (write! out (u8vector 3 4 5)))
      (test "Data correctly placed in raw vector" (u8vector 1 2 3) (subu8vector dest 5 8))))

  (test-group "2.2 Dynamic Expansion"
    (let ((dyn (make-bytevector-dynamic-output-port 4)))
      (write! dyn (u8vector 101 102 103 104 105))
      (test "Capacity doubled" #t (>= (get-buffer-capacity dyn) 8))
      (test "Position correct" 5 (get-position dyn))
      (truncate! dyn 2)
      (test "Truncate affects size/available" 2 (get-data-length dyn)))))

;; --- 3. Buffered Infrastructure (The "Heavy Lifters") ---
(test-group "3. Buffered Output Path"
  (let* ((sink (make-bytevector-dynamic-output-port))
         (b-out (make-buffered-output-port sink 10))) ;; Tiny 10-byte buffer
    
    (test "Small write stays in buffer (Sink empty)" 0 (get-data-length sink))
    (write! b-out (u8vector 1 2 3))

	(test-group "3.1 Bypass Optimization"
	  (write! b-out (make-u8vector 15 99))
	  ;; Use your API: get-output-u8vector returns the whole logical segment
	  (let ((result (get-output-u8vector sink)))
		(test "Bypass wrote large chunk directly to sink" 18 (u8vector-length result))
		(test "Check specific byte in bypassed data" 99 (u8vector-ref result 5))))))
	
(test-group "4. Buffered Input Path (Auto-Refill & Bypass)"
  (let* ((raw-data (u8vector 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
         (source (make-bytevector-input-port raw-data))
         (b-in (make-buffered-input-port source 5)) ;; 5-byte buffer
         (target (make-u8vector 20 0)))
    
    (test-group "4.1 Auto-Fill Logic"
      (test "Read-byte! triggers initial fill" 1 (read-byte! b-in))
      (test "Buffer now has remaining 4 bytes" 4 (available (slot-value b-in 'buffer))))

    (test-group "4.2 Tiered Bypass Logic"
      ;; We have 4 bytes in buffer. We ask for 10.
      ;; Logic: Drain 4 from buffer, Bypass read 6 from source.
      (let ((read-count (read! b-in target 0 10)))
        (test "Total bytes read matches request" 10 read-count)
        (test "Correct data sequence after bypass" 2 (u8vector-ref target 0))
        (test "Source cursor advanced correctly" 11 (get-position source))))
    
    (test-group "4.3 EOF Handling"
      (let ((final-read (read! b-in target 0 100))) ;; Read way past end
        (test "Handles EOF gracefully" 4 final-read)))))


(test-group "5. Single-Byte I/O (Specialized Methods)"
  
  (test-group "5.1 Bytevector Single-Byte"
    (let* ((data (u8vector 10 20 30))
           (in (make-bytevector-input-port data))
           (out (make-bytevector-output-port (make-u8vector 3 0))))
      (test "read-byte! matches sequence" 10 (read-byte! in))
      (test "read-byte! increments offset" 1 (get-position in))
      (write-byte! out 99)
      ;; Note: get-output-u8vector returns a copy of the written segment
      (test "write-byte! updates data" (u8vector 99) (get-output-u8vector out))
      (test "write-byte! updates size" 1 (get-data-length out))))

  (test-group "5.2 Buffered Auto-Refill (read-byte!)"
    (let* ((source-data (u8vector 1 2 3 4 5 6))
           (source (make-bytevector-input-port source-data))
           (b-in (make-buffered-input-port source 2))) ;; Tiny 2-byte buffer
      (test "First read-byte! triggers fill" 1 (read-byte! b-in))
      (test "Second read-byte! from existing buffer" 2 (read-byte! b-in))
      (test "Third read-byte! triggers second fill" 3 (read-byte! b-in))
      (test "Available reflects refill state" 1 (available (slot-value b-in 'buffer)))))

  (test-group "5.3 Buffered Auto-Flush (write-byte!)"
    (let* ((sink (make-bytevector-dynamic-output-port))
           (b-out (make-buffered-output-port sink 2))) ;; Tiny 2-byte buffer
      (write-byte! b-out 10)
      (write-byte! b-out 20)
      (test "Buffer full but not yet flushed" 0 (get-data-length sink))
      (write-byte! b-out 30) ;; Should trigger flush of [10, 20]
      (test "Sink received flushed data" 2 (get-data-length sink))
      (test "Flushed data is correct" (u8vector 10 20) (get-output-u8vector sink))
      (flush! b-out)
      (test "Final flush captures last byte" 3 (get-data-length sink))
      (test "Complete data correct" (u8vector 10 20 30) (get-output-u8vector sink))))

  (test-group "5.4 Interleaved read! and read-byte!"
    (let* ((source-data (u8vector 1 2 3 4 5 6 7 8 9 10))
           (source (make-bytevector-input-port source-data))
           (b-in (make-buffered-input-port source 4))
           (target (make-u8vector 2 0)))
      ;; Sequence of actions
      (read-byte! b-in)       ;; Reads 1, manager offset 1
      (read! b-in target 0 2) ;; Reads 2, 3, manager offset 3
      (test "Data from interleaved read! is correct" 
      (u8vector->list (u8vector 2 3)) 
      (u8vector->list target))
      (test "Next read-byte! is correct" 4 (read-byte! b-in))
      (test "Buffer manager now empty" 0 (available (slot-value b-in 'buffer)))))
  )

(test-group "6. Raw File I/O"
  (let* ((filename "test_io.bin")
         (test-data (u8vector 65 66 67 68 69)) ;; "ABCDE"
         (out-raw (make-file-output-port filename (bitwise-ior open/write open/create open/truncate)))
         (out-buf (make-buffered-output-port out-raw 2))) ;; Tiny buffer to force flushes
    
    (write! out-buf test-data)
    (close out-buf) ;; Should flush and close FD
    
    (let* ((in-raw (make-file-input-port filename open/read))
           (in-buf (make-buffered-input-port in-raw 2))
           (result (make-u8vector 5 0)))
      (read! in-buf result)
      (test "File round-trip matches" (u8vector->list test-data) (u8vector->list result))
      (close in-buf))))

(test-group "7. FFI Locative & Offset Validation"
  (let* ((filename "locative_test.bin")
         ;; Create a file containing [10, 20, 30]
         (out (make-file-output-port filename (bitwise-ior open/write open/create open/truncate)))
         (dummy-data (u8vector 10 20 30)))
    
    (write! out dummy-data)
    (close out)

    (let* ((in (make-file-input-port filename open/read))
           ;; Target vector initialized to zeros
           (target (make-u8vector 5 0)))
      
      ;; READ TEST: Read 3 bytes from file into 'target' starting at index 2
      ;; Target should become: #u8(0 0 10 20 30)
      (read! in target 2 3)
      
      (test "Data lands at correct offset via locative" 
            '(0 0 10 20 30) 
            (u8vector->list target))
      (close in))))


(test-group "8. Base <file-port> Finalizer"
  (let ((port-ptr #f))
    (let ((p (make-file-input-port "locative_test.bin" open/read)))
      (set! port-ptr p)
      (test "Port starts open" #f (closed? p)))
    
    (set! port-ptr #f)
    ;; Trigger a full GC to reap the orphaned port object
    (import (only (chicken gc) gc))
    (gc #t) 
    
    (let ((p2 (make-file-input-port "locative_test.bin" open/read)))
      (close p2)
      (test "Manual close neutralizes finalizer safely" #t (closed? p2)))))


(test-end "Coop-Ports")
