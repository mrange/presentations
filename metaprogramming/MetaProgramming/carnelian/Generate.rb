# ----------------------------------------------------------------------------------------------
# Copyright (c) Mårten Rånge.
# ----------------------------------------------------------------------------------------------
# This source code is subject to terms and conditions of the Microsoft Public License. A
# copy of the license can be found in the License.html file at the root of this distribution.
# If you cannot locate the  Microsoft Public License, please send an email to
# dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
#  by the terms of the Microsoft Public License.
# ----------------------------------------------------------------------------------------------
# You must not remove this notice, or any other, from this software.
# ----------------------------------------------------------------------------------------------

require 'carnelian/executor'

def mapDefinition(name, key_type, value_type, boxed_key_type=nil, boxed_value_type=nil)
  {
    name:             name              ,
    key_type:         key_type          ,
    value_type:       value_type        ,
    boxed_key_type:   boxed_key_type    ,
    boxed_value_type: boxed_value_type  ,
  }
end

$model =
  {
    imports:
      [
        "java.util.ArrayList"            ,
      ]                                  ,
    mapDefinitions:
      [
#                      MapName                     KeyType   ValueType             BoxKeyType  BoxValueType
        mapDefinition("ValueGroupMap"           , "int"   , "long"              , "Integer" , "Long"),
        mapDefinition("InstanceGroupMap"        , "long"  , "String"            , "Long"    , nil   ),
        mapDefinition("ClassGroupMap"           , "int"   , "ClassGroup"        , "Integer" , nil   ),
        mapDefinition("InstanceNameMap"         , "String", "Instance"          , nil       , nil   ),
        mapDefinition("InstanceIdMap"           , "long"  , "Instance"          , "Long"    , nil   ),
#        mapDefinition("ClassInstanceNameMap"    , "String", "PmClassInstance"   , nil       , nil   ),
        mapDefinition("ClassInstanceIdMap"      , "int"   , "ClassInstance"     , "Integer" , nil   ),
        mapDefinition("DefinitionNameMap"       , "String", "Definition"        , nil       , nil   ),
        mapDefinition("DefinitionIdMap"         , "int"   , "Definition"        , "Integer" , nil   ),
        mapDefinition("ClassDefinitionNameMap"  , "String", "ClassDefinition"   , nil       , nil   ),
        mapDefinition("ClassDefinitionIdMap"    , "int"   , "ClassDefinition"   , "Integer" , nil   ),
        mapDefinition("RunnableMap"             , "int"   , "Runnable"          , "Integer" , nil   ),
        mapDefinition("ListenerMap"             , "int"   , "ArrayList"         , "Integer" , nil   ),
        mapDefinition("SessionMap"              , "long"  , "long"              , "Long"    , "Long"),
      ]
  }

CarnelianExecutor.execute_metaprogram_to_file "MapView.mp"      , "Maps.java"
CarnelianExecutor.execute_metaprogram_to_file "TestMapView.mp"  , "TestMaps.java"
