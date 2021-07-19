import pandas as pd
import numpy as np
import pickle
import gcsfs
from flask import Flask,request
import datetime
from sklearn.preprocessing import LabelEncoder


app = Flask(__name__)

loaded_model=pickle.load(open('model.pckl','rb'))



@app.route('/')
def root_response():
    return "Hello"

@app.route('/loan-prediction', methods=['POST'])
def api_response():
     data=request.get_json()
     datapoint = pd.io.json.json_normalize(data)
     datapoint['Total_Income']=datapoint['ApplicantIncome'] + datapoint['CoapplicantIncome']
         # apply log transformation to the attribute
     datapoint['ApplicantIncomeLog'] = np.log(datapoint['ApplicantIncome'])
     datapoint['CoapplicantIncomeLog'] = np.log(datapoint['CoapplicantIncome'])
     datapoint['LoanAmountLog'] = np.log(datapoint['LoanAmount'])
     datapoint['Loan_Amount_Term_Log'] = np.log(datapoint['Loan_Amount_Term'])
     datapoint['Total_Income_Log'] = np.log(datapoint['Total_Income'])
     cols = ['ApplicantIncome', 'CoapplicantIncome', "LoanAmount", "Loan_Amount_Term", "Total_Income", 'Loan_ID',
            'CoapplicantIncomeLog']
     df = datapoint.drop(columns=cols, axis=1)
     cols = ['Gender', "Married", "Education", 'Self_Employed', "Property_Area", "Dependents"]
     le = LabelEncoder()
     for col in cols:
         df[col] = le.fit_transform(df[col])
 #    fs = gcsfs.GCSFileSystem(project='brooklyn-hospital-dev')
 #    Pkl_Filename = "gs://kafka-data-tbh/serverless/model.pckl"
     prediction = loaded_model.predict(df)
     xx=np.array_str(prediction)
     if xx =='[1]':
         return 'Loan is sanctioned'
     elif xx=='[0]':
         return 'loan is declined'
        

     

if __name__=='__main__':
    app.run(host='0.0.0.0',port=8118,debug=False)
    
     