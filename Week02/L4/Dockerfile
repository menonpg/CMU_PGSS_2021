FROM python:3.7.6
COPY . /app
WORKDIR /app
RUN apt-get update
RUN apt-get install -y python3-pip
RUN pip3 install -r requirements.txt 
RUN chmod +rwx loan-prediction.py
ENTRYPOINT ["python"]
CMD ["loan-prediction.py"]
EXPOSE 8118