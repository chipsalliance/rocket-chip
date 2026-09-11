package cf_math_pkg;
  function automatic integer idx_width(input integer n);
    if (n <= 1) idx_width = 1;
    else idx_width = $clog2(n);
  endfunction
endpackage
