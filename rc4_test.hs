import Test.HUnit
import RC4

-- Test vectors from the Wikipedia RC4 article: https://en.wikipedia.org/wiki/RC4#Test_vectors
encryption_test1 = rc4 "Key" "Plaintext" ~?= "BBF316E8D940AF0AD3"
encryption_test2 = rc4 "Wiki" "pedia" ~?= "1021BF0420"
encryption_test3 = rc4 "Secret" "Attack at dawn" ~?= "45A01F645FC35B383552544B9BF5"
encryption_tests = TestList [ encryption_test1, encryption_test2, encryption_test3 ]

ciphertext1 = fromHex (rc4 "Key" "Plaintext")
descryption_test1 = fromHex (rc4 "Key" ciphertext1) ~?= "Plaintext"

ciphertext2 = fromHex (rc4 "Wiki" "pedia")
descryption_test2 = fromHex (rc4 "Wiki" ciphertext2) ~?= "pedia"

ciphertext3 = fromHex (rc4 "Secret" "Attack at dawn")
descryption_test3 = fromHex (rc4 "Secret" ciphertext3) ~?= "Attack at dawn"

descryption_tests = TestList [ descryption_test1, descryption_test2, descryption_test3 ]

main = runTestTT $ TestList [ encryption_tests, descryption_tests ]