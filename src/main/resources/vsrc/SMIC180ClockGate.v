// SMIC SCC018UG UHD integrated clock gate for positive-edge clock domains.
module SMIC180ClockGate (
  output out,
  input  en,
  input  test_en,
  input  in
);
  CLKLANQUHDV1 u_icg (
    .Q  (out),
    .CK (in),
    .E  (en),
    .TE (test_en)
  );
endmodule
