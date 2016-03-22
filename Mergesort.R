Merge_a <- function(a,b) {
  r<-numeric(length(a)+length(b))
  ai<-1; bi<-1; j<-1;
  for(j in 1:length(r)) {
    if((ai<=length(a) && a[ai]<b[bi]) || (bi>length(b))) {
          r[j]<-a[ai]
          ai<-ai+1
      } else {
          r[j]<-b[bi]
          bi<-bi+1
      }
  }
  r
}

Mergesort_a <- function(l) {
  if (length(l)>1) {
    n<-length(l)
    half<-ceiling(n/2)
    Merge_a(Mergesort_a(l[1:half]),Mergesort_a(l[(half+1):n]))
  } else {
    l
  }
}


Csplitinv_Merge_a <- function(a,b) {
  ar<-length(a)
  br<-length(b)
  r<-numeric(ar+br)
  ai<-1; bi<-1; j<-1;
  counter<-0
  for(j in 1:length(r)) {
    if((ai<=length(a) && a[ai]<b[bi]) || (bi>length(b))) {
      r[j]<-a[ai]
      ai<-ai+1
    } else {
      r[j]<-b[bi]
      bi<-bi+1
      counter<-counter+ar-ai+1
    }
  }
  counter
}

Cinv_Mergesort_a <- function(l) {
  n<-length(l)
  if (n>1) {
    half<-ceiling(n/2)
    x<-Cinv_Mergesort_a(l[1:half])
    y<-Cinv_Mergesort_a(l[(1+half):n])
    z<-Csplitinv_Merge_a(l[1:half],l[(1+half):n])
    x+y+z
  } else {
    0
  }
}

Lista<-as.matrix(read.table("IntegerArray.txt"))