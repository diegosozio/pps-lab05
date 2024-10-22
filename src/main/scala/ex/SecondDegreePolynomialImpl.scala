package ex

case class SecondDegreePolynomialImpl(
                                       secondDegree: Double,
                                       firstDegree: Double,
                                       constant: Double
                                     ) extends SecondDegreePolynomial {
  def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial = {
    SecondDegreePolynomialImpl(
      this.secondDegree + polynomial.secondDegree,
      this.firstDegree + polynomial.firstDegree,
      this.constant + polynomial.constant
    )
  }

  def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomialImpl = {
    SecondDegreePolynomialImpl(
      this.secondDegree - polynomial.secondDegree,
      this.firstDegree - polynomial.firstDegree,
      this.constant - polynomial.constant
    )
  }
}
