#import key
import os
os.environ["HUGGINGFACEHUB_API_TOKEN"] = "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"

#load packages
from langchain_community.document_loaders.csv_loader import CSVLoader
from langchain_community.document_loaders import TextLoader
from langchain.chains import LLMChain
from langchain_community.llms import HuggingFaceHub
from langchain.indexes import VectorstoreIndexCreator
from langchain_community.embeddings import HuggingFaceHubEmbeddings
from langchain.chains import RetrievalQA
from langchain_community.vectorstores import FAISS
from langchain.prompts import PromptTemplate

#load model
llm=HuggingFaceHub(repo_id="mistralai/Mixtral-8x7B-Instruct-v0.1", model_kwargs={"temperature":0.1, "max_new_tokens":4500})

#create index and run query
def load_doc(x,y):
  loader = TextLoader(x)
  documents = loader.load()
  embeddings = HuggingFaceHubEmbeddings()
  vectorstore = FAISS.from_documents(documents, embeddings)
  chain=RetrievalQA.from_chain_type(llm=llm, chain_type="stuff", retriever=vectorstore.as_retriever())
  #index = VectorstoreIndexCreator(embedding=HuggingFaceHubEmbeddings()).from_loaders([loader])
  # chain = RetrievalQA.from_llm(
  # llm=llm,
  # retriever=index.vectorstore.as_retriever()
  # )
  z = str(chain.run(y))
  return z 

#ask LLM straight
template = """Question: {question}

Answer: Keep it short. Do not guess."""
prompt = PromptTemplate(template=template, input_variables=["question"])
llm_chain = LLMChain(
    prompt=prompt,
    llm=llm
)

def ask_q(x):
    z = str(llm_chain.run(x))
    return z

