import Test.HUnit
import RC4

-- Test vectors from the Wikipedia RC4 article: https://en.wikipedia.org/wiki/RC4#Test_vectors
ciphertext1 = rc4 "Key" "Plaintext"
ciphertext2 = rc4 "Wiki" "pedia"
ciphertext3 = rc4 "Secret" "Attack at dawn"

encryption_test1 = ciphertext1 ~?= "BBF316E8D940AF0AD3"
encryption_test2 = ciphertext2 ~?= "1021BF0420"
encryption_test3 = ciphertext3 ~?= "45A01F645FC35B383552544B9BF5"
encryption_tests = TestList [ encryption_test1, encryption_test2, encryption_test3 ]

descryption_test1 = fromHex (rc4 "Key" (fromHex ciphertext1)) ~?= "Plaintext"
descryption_test2 = fromHex (rc4 "Wiki" (fromHex ciphertext2)) ~?= "pedia"
descryption_test3 = fromHex (rc4 "Secret" (fromHex ciphertext3)) ~?= "Attack at dawn"
descryption_tests = TestList [ descryption_test1, descryption_test2, descryption_test3 ]

main = runTestTT $ TestList [ encryption_tests, descryption_tests ]