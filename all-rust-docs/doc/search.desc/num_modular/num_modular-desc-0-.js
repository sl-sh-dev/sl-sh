searchState.loadedDescShard("num_modular", 0, "This crate provides efficient Modular arithmetic …\nThe underlying representation type of the integer\nUtility function for exact division, with precomputed …\nA modular reducer for (pseudo) Mersenne numbers <code>2^P - K</code> as …\nAn integer in modulo ring with a fixed (pseudo) Mersenne …\nProvides a utility function to convert signed integers …\nCore modular arithmetic operations.\nRepresents an number defined in a modulo ring ℤ/nℤ\nCollection of common modular arithmetic operations\nModular power functions\nCollection of operations similar to ModularOps, but takes …\nMath symbols related to modular arithmetics\nCore unary modular arithmetics\nA modular reducer based on Montgomery form, only supports …\nAn integer in modulo ring based on Montgomery form\nDivide a DoubleWord by a prearranged divisor.\nDivide a 3-Word by a prearranged DoubleWord divisor.\nPre-computing the modular inverse for fast divisibility …\nDivide a Word by a prearranged divisor.\nA wrapper of Normalized2by1Divisor that can be used as a …\nA wrapper of Normalized3by2Divisor that can be used as a …\nAn integer in a modulo ring\nA modular reducer that can ensure that the operations on …\nA plain reducer that just use normal Rem operators. It …\nAn integer in modulo ring based on conventional Rem …\nReturn self % m, but accepting signed integers\nCalculate (lhs + rhs) mod m in reduced form\nReturn (self + rhs) % m\nCheck whether target is a valid reduced form\nCalculate Jacobi Symbol (a|n), where a is <code>self</code>. Returns …\nCalculate Legendre Symbol (a|n), where a is <code>self</code>. Returns …\nMultiplication of double width and single width\nConvert an normal integer into the same ring.\nCalculate 2*target mod m\nCalculate modular double ( x+x mod m)\nCheck if d divides self with the help of the …\n(a / divisor, a % divisor)\n(a / divisor, a % divisor)\n(a / divisor, a % divisor)\n(a / divisor, a % divisor)\n(a / divisor, a % divisor)\nReturns (a / divisor, a % divisor)\nReturns (a / divisor, a % divisor)\nReturns (a / divisor, a % divisor)\nReturns (a / divisor, a % divisor)\nReturns (a / divisor, a % divisor)\nReturns (a / divisor, a % divisor) The result must fit in …\nReturns (a / divisor, a % divisor) The result must fit in …\nReturns (a / divisor, a % divisor) The result must fit in …\nReturns (a / divisor, a % divisor) The result must fit in …\nReturns (a / divisor, a % divisor) The result must fit in …\nThe input a is arranged as (lo, mi &amp; hi) The output is (a …\nThe input a is arranged as (lo, mi &amp; hi) The output is (a …\nThe input a is arranged as (lo, mi &amp; hi) The output is (a …\nThe input a is arranged as (lo, mi &amp; hi) The output is (a …\nThe input a is arranged as (lo, mi &amp; hi) The output is (a …\nDivdide a 4-word number with double word divisor\nDivdide a 4-word number with double word divisor\nDivdide a 4-word number with double word divisor\nDivdide a 4-word number with double word divisor\nDivdide a 4-word number with double word divisor\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nGet the <strong>normalized</strong> divisor.\nCalculate the value of self + self\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nReturns the argument unchanged.\nMost significant part\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalls <code>U::from(self)</code>.\nCalculate target^-1 mod m in reduced form, it may return …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate the inverse m &gt; 0 of a normalized divisor (fit …\nCalculate modular inverse (x such that self*x = 1 mod m).\nCheck if the integer is zero\nTest if the residue() == 0\nCalculate Jacobi Symbol (a|n), where a is <code>self</code>\nCalculate Kronecker Symbol (a|n), where a is <code>self</code>\nCalculate Legendre Symbol (a|n), where a is <code>self</code>.\nLeast significant part\nReturn the modulus of the ring\nGet the modulus in original integer type\nCalculate (lhs * rhs) mod m in reduced form\nReturn (self * rhs) % m\nCalculate -monty mod m in reduced form\nReturn (-self) % m and make sure the result is normalized …\nCreate a reducer for a modulus m\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nInitialize from a given normalized divisor.\nConstruct the preinv instance with raw values.\nConstruct the preinv instance with raw values.\nConstruct the preinv instance with raw values.\nConstruct the preinv instance with raw values.\nConstruct the preinv instance with raw values.\nConvert n into the modulo ring ℤ/mℤ (i.e. <code>n % m</code>)\nMultiplication of double width and single width\nCalculate base ^ exp mod m in reduced form\nReturn (self ^ exp) % m\nReturn the normalized residue of this integer in the ring\nTransform a reduced form back to normal integer\nCalculate modular square ( x*x mod m )\nCalculate target^2 mod m in reduced form\nCalculate the value of self * self\nCalculate (lhs - rhs) mod m in reduced form\nReturn (self - rhs) % m\nTransform a normal integer into reduced form\nA double width integer type based on the largest built-in …\nAlias of the builtin integer type with max width …\nCalculate multiplication of two umax integers with result …\nOptimized squaring function for umax integers")