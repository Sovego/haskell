data AOBtype = O | A | B | AB deriving (Eq)
instance Show AOBtype where
    show O = "I"
    show A = "II"
    show B = "III"
    show AB = "IV"
data Rhtype = Pos | Neg deriving (Eq)
instance Show Rhtype where
    show Pos = "+"
    show Neg = "-"
data BloodType = BloodType {
        aob::AOBtype,
        rh::Rhtype
    } deriving (Eq)
instance Show BloodType where
    show (BloodType aob rh) = show aob ++ show rh
data Patient = Patient {
    name::String,
    age::Int,
    sex::String,
    height::Int,
    blood::BloodType
} 
instance Show Patient where
    show (Patient name age sex height blood)= "***********\n" 
                                              ++ "Name: " ++ name ++ "\n" 
                                              ++ "Age: " ++ show age ++ "\n" 
                                              ++ "Sex: " ++ sex ++ "\n" 
                                              ++ "Heignt: " ++show height ++ "\n" 
                                              ++ "Blood: " ++ show blood ++ "\n" 
                                              ++  "***********\n"   
rhcompatibility rh1 rh2 | rh1 == Neg = True
                        | rh1 == Pos && rh2 == Pos = True
                        | otherwise = False
aobcompatibility aob1 aob2 | aob1 == O = True  
donate (Patient _ _ _ _ (BloodType aob1 rh1)) (Patient _ _ _ _ (BloodType aob2 rh2)) | aob1 == O && rh1 == Neg = True
                                                                                     | (aob1 == O && rh1 == Pos) && (rh2 == Pos) = True
                                                                                     | (aob1== A && rh1 == Neg) && ((aob2==A && rh2==Neg) || (aob2==A && rh2==Pos) || (aob2==AB && rh2==Neg) || (aob2==AB && rh2==Pos)) = True
                                                                                     | (aob1== A && rh1 == Pos) && ((aob2== A && rh2 == Pos)||(aob2== AB && rh2 == Pos)) = True
                                                                                     | (aob1== B && rh1 == Neg) && ((aob2==B && rh2==Neg) || (aob2==B && rh2==Pos) || (aob2==AB && rh2==Neg) || (aob2==AB && rh2==Pos)) = True
                                                                                     | (aob1== B && rh1 == Pos) && ((aob2==B && rh2 == Pos)||(aob2== AB && rh2 == Pos)) = True
                                                                                     | (aob1== AB && rh1 == Neg) && ((aob2==AB && rh2 == Neg)||(aob2== AB && rh2 == Pos)) = True
                                                                                     | (aob1== AB && rh1 == Pos) && (aob2==AB && rh2==Pos) = True
                                                                                     | otherwise = False
-- let a = Patient{name="Test",age=5,sex="M",height=50,blood=BloodType{aob=O,rh=Pos}} // первая положительная
-- let b = Patient{name="Test",age=5,sex="M",height=50,blood=BloodType{aob=A,rh=Pos}} // вторая положительная
-- let b = Patient{name="Test",age=5,sex="M",height=50,blood=BloodType{aob=B,rh=Neg}} // третья негативная