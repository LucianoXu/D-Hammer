# Generation Task
You should generate examples of Dirac notation equations, written in LaTex format. Here are a list of examples:

  \item(\text{lftrace\_baseE})  $\tr(A) = \sum_{i \in \mathbf{U}}\bra{i} A \ket{i}$
  \item(\text{lftraceC})  $\tr(A B) = \tr(B A)$
  \item(\text{lftrace\_is\_linear})  $\tr(c A + B) = c\ \tr(A) + \tr(B)$
  \item(\text{lftrace\_adj})  $\tr(A^\dagger) = \tr(A)^*$
  \item(\text{lftrace\_tr})  $\tr(A^T) = \tr(A)$
  \item(\text{lftrace\_conj})  $\tr(A^*) = \tr(A)^*$
  % \item(\text{outp_trlf})  $\tr(\ket{u}\bra{v}) = \braket{v|u}$ ($u$, $v$ are basis vectors)
  \item(\text{outp\_trlf})  $\tr(\ket{u}\bra{v}) = \braket{v|u}$ ($u$, $v$ are arbitrary vectors)
  \item(\text{sumeb\_out})  $\sum_{i\in\mathbf{U}} \ket{i}\bra{i} = I$
  \item(\text{delta\_lf\_eb}) $|i\>\<j|\cdot |k\> = \delta_{k,j}|i\>$
  \item(\text{comp\_delta\_lf\_cond})  $\ket{i}\bra{j}\cdot\ket{k}\bra{l} = \delta_{j,k}\ket{i}\bra{l}$
  \item(\text{comp\_delta\_lf})  $\ket{i}\bra{j}\cdot\ket{j}\bra{k} = \ket{i}\bra{k}$
  \item(\text{trlf\_deltar})  $\tr(A \ket{i}\bra{j}) = \bra{j} (A \ket{i})$
  \item(\text{lfun\_sum\_delta})  $A = \sum_{j\in\mathbf{U}}\sum_{i\in\mathbf{U}} \bra{i}(A\ket{j}) \ket{i}\bra{j}$
  \item(\text{onb\_dot(CB)}) $\<i|j\> = \delta_{i,j}$
  \item(\text{onb\_vec(CB)})  $\ket{v} = \sum_{i\in\mathbf{U}}(\braket{i|v})\ket{i}$
  \item(\text{outp\_complV})  $(A\ket{u})\bra{v} = A(\ket{u}\bra{v})$
  \item(\text{outp\_comprV})  $\ket{u}(A\ket{v})^\dagger = (\ket{u}\bra{v})A^\dagger$
  \item(\text{onb\_lfun(CB)}) $A = \sum_{i\in \mathbf{U}}(A|i\>)\<i|$
  \item(\text{sumonb\_out\_bool(CB)} $|0\>\<0| + |1\>\<1| = I$
  \item(\text{ponb\_ns(CB)} $\<i|i\> = 1$


## Requirements
These examples should be as unique as possible. Output them in LaTeX format.