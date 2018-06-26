package edu.amrita.cb.cen.nlp.astra.util
import scala.xml._

class TAGml {

 def tagSchema = {
  //<?xml version="1.0"?>

  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
   <xs:element name="xtag">
    <xs:complexType>
     <xs:attribute name="language" type="xs:string" default="English"/>
     <xs:attribute name="type" type="xs:string" default="Wide Coverage"/>
     <xs:sequence minOccurs="0" maxOccurs="1">
      <xs:element name="nonterminals" minOccurs="1" maxOccurs="1">
       <xs:complexType>
        <xs:element name="symbol">
         <xs:complexType>
          <xs:attribute name="name" type="xs:string" use="required"/>
         </xs:complexType>
        </xs:element>
       </xs:complexType>
      </xs:element>
      <xs:element name="trees" minOccurs="1" maxOccurs="1">
       <xs:complexType>
        <xs:element name="tree" minOccurs="1" maxOccurs="unbounded">
         <xs:complexType>
          <xs:attribute name="name" type="xs:string" use="required"/>
          <xs:attribute name="family" type="xs:string" use="required"/>
          <xs:attribute name="pos" type="xs:string" use="required"/>
          <xs:attribute name="type" type="xs:string" use="required"/>
          <xs:attribute name="minimal" type="xs:string" default="yes"/>
          <xs:attribute name="id" type="xs:nonNegativeInteger" use="required"/>
          <xs:all>
           <xs:element name="comment" type="xs:string"/>
           <xs:element name="notation" type="xs:string" minOccurs="1"/>
           <xs:element name="unification">
            <xs:complexType>
             <xs:element name="eq">
              <xs:complexType>
               <xs:attribute name="key" type="xs:string" use="required"/>
               <xs:attribute name="value" type="xs:string" use="required"/>
              </xs:complexType>
             </xs:element>
            </xs:complexType>
           </xs:element>
           <xs:complexType name="nodeType">
            <xs:attribute name="name" type="xs:string" use="required"/>
            <xs:attribute name="name" type="xs:string" use="required"/>
            <xs:attribute name="name" type="xs:string" use="required"/>
            <xs:attribute name="name" type="xs:string" use="required"/>
            <xs:attribute name="name" type="xs:string" use="required"/>
            <xs:sequence>
             <xs:element name="fs" minOccurs="0" maxOccurs="1">
              <xs:complexType>
               <xs:complexType name="featureType">
                <xs:attribute name="key" type="xs:string" use="required"/>
                <xs:attribute name="value" type="xs:string" use="required"/>
               </xs:complexType>
               <xs:element name="top" minOccurs="0" maxOccurs="1">
                <xs:complexType>
                 <xs:element name="f" type="featureType" minOccurs="1" maxOccurs="unbounded"/>
                </xs:complexType>
               </xs:element>
               <xs:element name="bottom" minOccurs="0" maxOccurs="1">
                <xs:complexType>
                 <xs:element name="f" type="featureType" minOccurs="1" maxOccurs="unbounded"/>
                </xs:complexType>
               </xs:element>
              </xs:complexType>
             </xs:element>
             <xs:element name="node" type="nodeType" minOccurs="0" maxOccurs="unbounded"/>
            </xs:sequence>
           </xs:complexType>
           <xs:element name="nodes" minOccurs="1">
            <xs:complexType>
             <xs:element name="node" type="nodeType" minOccurs="1" maxOccurs="unbounded"/>
            </xs:complexType>
           </xs:element>
          </xs:all>
         </xs:complexType>
        </xs:element>
       </xs:complexType>
      </xs:element>
     </xs:sequence>
    </xs:complexType>
   </xs:element>
  </xs:schema>
 }

}