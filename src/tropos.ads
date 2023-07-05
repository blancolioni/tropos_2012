private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Equal_Case_Insensitive;
private with Ada.Strings.Hash_Case_Insensitive;

with Ada.Iterator_Interfaces;

package Tropos is

   type Configuration is tagged private
   with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Configuration;

   Empty_Config : constant Configuration;

   function New_Config (Name : String) return Configuration;
   function New_Config (Index : Integer) return Configuration;

   procedure Set_Attribute
     (Config : in out Configuration;
      Name   : String;
      Value  : String);

   function Attribute
     (Config : Configuration;
      Name   : String)
      return String;

   procedure Iterate_Attributes
     (Config  : Configuration;
      Process : not null access
        procedure (Name : String;
                   Value : String));

   procedure Add (To_Config : in out Configuration;
                  Child     :        Configuration);

   procedure Add (To_Config : in out Configuration;
                  Name      :        String;
                  Value     :        String);

   procedure Add (To_Config : in out Configuration;
                  Name      :        String;
                  Value     :        Integer);

   procedure Add (To_Config : in out Configuration;
                  Name      :        String;
                  Value     :        Float);

   procedure Add (To_Config : in out Configuration;
                  Name      :        String;
                  Value     :        Long_Float);

   procedure Set_Path
     (Config         : in out Configuration;
      Path           : String;
      Value          : String;
      Path_Separator : Character := '.');

   function Has_Children (This : Configuration) return Boolean;
   function Is_Simple_Value (This : Configuration) return Boolean;

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return String;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : String)
                 return String;

   function Get (From_Config   : Configuration;
                 Field_Name    : String)
                 return Integer;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Integer)
                 return Integer;

   function Get (From_Config   : Configuration;
                 Field_Name    : String)
                 return Float;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Float)
                 return Float;

   function Get (From_Config   : Configuration;
                 Field_Name    : String)
                 return Long_Float;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Long_Float)
                 return Long_Float;

   function Get (From_Config   : Configuration;
                 Field_Name    : String)
                 return Boolean;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Boolean)
                 return Boolean;

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return String;

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Integer;

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Float;

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Long_Float;

   function Value (Of_Config : Configuration)
                   return String;

   function Value
     (Of_Config     : Configuration;
      Default_Value : Integer := 0)
      return Integer;

   function Value (Of_Config     : Configuration;
                   Default_Value : Float := 0.0)
                   return Float;

   function Value (Of_Config     : Configuration;
                   Default_Value : Long_Float := 0.0)
                   return Long_Float;

   generic
      type Field_Type is private;
      with function From_String (Text : String) return Field_Type;
   procedure Configure
     (Config   :        Configuration;
      Name     :        String;
      Field    :    out Field_Type);

   generic
      type Structure_Type is private;
      with procedure Configure (Item    : in out Structure_Type;
                                Config  :        Configuration)
         is <>;
   procedure Configure_Structure
     (Config    :        Configuration;
      Name      :        String;
      Structure :    out Structure_Type);

   procedure Configure_Container
     (Config    :        Configuration;
      Add       : not null access procedure (Config : Configuration));

   generic
      type Enum is (<>);
   function Get_Enum
     (Config   :        Configuration;
      Name     :        String)
      return Enum;

   generic
      type Enum is (<>);
      Default : Enum;
   function Get_Enum_With_Default
     (Config   :        Configuration;
      Name     :        String)
      return Enum;

   function Config_Name
     (Item : Configuration)
     return String;

   type Cursor is private;
   No_Element : constant Cursor;

   function First (Item : Configuration) return Cursor;
   function Element (Item : Cursor) return Configuration;
   procedure Next (Item : in out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   package Configuration_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
      (Element : not null access constant Configuration) is
   private
   with
      Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Configuration) is private
   with
      Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Configuration;
      Position  : Cursor) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Configuration;
      Position  : Cursor) return Reference_Type;
   pragma Inline (Reference);

   function Child (Of_Config  : Configuration;
                   Child_Name : String)
                   return Configuration;
   --  Return the configuration associated with the first child of the given
   --  config, or an empty configuration is such a child does not exist

   function Required_Child
     (Of_Config  : Configuration;
      Child_Name : String)
      return Configuration;
   --  As child, but raise an exception instead of returning an empty
   --  configuration if no child with that name exists

   function Child (Of_Config  : Configuration;
                   Index      : Positive)
                   return Configuration;

   function Contains (Config : Configuration;
                      Name   : String)
                      return Boolean;

   function Child_Count (Config : Configuration) return Natural;

   procedure Iterate
     (Config   : Configuration;
      Child_Name : String;
      Process    : not null access procedure (Position : Cursor));

   function Iterate
     (Container : Configuration)
      return Configuration_Iterator_Interfaces.Reversible_Iterator'Class;

   type Configuration_Array is array (Positive range <>) of Configuration;

   function Children (Config : Configuration;
                      Name   : String)
                      return Configuration_Array;

private

   type Configuration_Access is access all Configuration;

   package Configuration_Vector is
      new Ada.Containers.Vectors (Positive, Configuration_Access);

   package Attribute_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   type Configuration is tagged
      record
         Name       : Ada.Strings.Unbounded.Unbounded_String;
         Attributes : Attribute_Maps.Map;
         Children   : Configuration_Vector.Vector;
      end record;

   type Constant_Reference_Type
     (Element : not null access constant Configuration) is null record;

   type Reference_Type
     (Element : not null access Configuration) is null record;

   type Cursor is
      record
         Position : Configuration_Vector.Cursor;
      end record;

   Empty_Config : constant Configuration := (others => <>);

   No_Element : constant Cursor :=
                  (Position => Configuration_Vector.No_Element);

   function Attribute
     (Config : Configuration;
      Name   : String)
      return String
   is (if Config.Attributes.Contains (Name)
       then Config.Attributes.Element (Name)
       else "");

   function Has_Children (This : Configuration) return Boolean
   is (not This.Children.Is_Empty);

   function Is_Simple_Value (This : Configuration) return Boolean
   is (This.Has_Children
       and then This.Child_Count = 1
       and then This.Children.First_Element.Children.Is_Empty);

end Tropos;
