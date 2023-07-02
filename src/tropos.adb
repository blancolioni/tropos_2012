with Ada.Numerics.Float_Random;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

package body Tropos is

   Gen : Ada.Numerics.Float_Random.Generator;

   type Iterator is
     new Configuration_Iterator_Interfaces.Reversible_Iterator
   with record
      Container : Configuration_Access;
      Current   : Cursor;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   function To_Float_Value
     (Text : String)
      return Float;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function Equal (X, Y : Ada.Strings.Unbounded.Unbounded_String)
                   return Boolean
                   renames Ada.Strings.Unbounded.Equal_Case_Insensitive;

   ---------
   -- Add --
   ---------

   procedure Add
     (To_Config : in out Configuration;
      Child     :        Configuration)
   is
   begin
      To_Config.Children.Append
        (new Configuration'(Child));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (To_Config : in out Configuration;
      Name      :        String;
      Value     :        String)
   is
      Child_Name  : Configuration := New_Config (Name);
      Child_Value : constant Configuration :=
        New_Config (Value);
   begin
      Child_Name.Add (Child_Value);
      To_Config.Add (Child_Name);
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (To_Config : in out Configuration;
                  Name      :        String;
                  Value     :        Integer)
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      To_Config.Add (Name, Trim (Integer'Image (Value), Left));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (To_Config : in out Configuration;
                  Name      :        String;
                  Value     :        Float)
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      To_Config.Add (Name, Trim (Float'Image (Value), Left));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (To_Config : in out Configuration;
                  Name      :        String;
                  Value     :        Long_Float)
   is
      use Ada.Strings, Ada.Strings.Fixed;
   begin
      To_Config.Add (Name, Trim (Long_Float'Image (Value), Left));
   end Add;

   -----------
   -- Child --
   -----------

   function Child (Of_Config  : Configuration;
                   Child_Name : String)
                  return Configuration
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in 1 .. Of_Config.Children.Last_Index loop
         declare
            Child_Config : constant Configuration_Access :=
                             Of_Config.Children.Element (I);
         begin
            if Equal (Child_Config.Name, +Child_Name) then
               return Of_Config.Children.Element (I).all;
            end if;
         end;
      end loop;
      return New_Config (Child_Name);
   end Child;

   -----------
   -- Child --
   -----------

   function Child (Of_Config  : Configuration;
                   Index      : Positive)
                  return Configuration
   is
   begin
      return Of_Config.Children.Element (Index).all;
   end Child;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Config : Configuration) return Natural is
   begin
      return Config.Children.Last_Index;
   end Child_Count;

   --------------
   -- Children --
   --------------

   function Children (Config : Configuration;
                      Name   : String)
                      return Configuration_Array
   is
      Result : Configuration_Array (1 .. Config.Child_Count);
      Count  : Natural := 0;
   begin
      for I in Result'Range loop
         if Equal (Config.Children.Element (I).Name, +Name) then
            Count := Count + 1;
            Result (Count) := Config.Children.Element (I).all;
         end if;
      end loop;
      return Result (1 .. Count);
   end Children;

   -----------------
   -- Config_Name --
   -----------------

   function Config_Name
     (Item : Configuration)
     return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Config_Name;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Config   :        Configuration;
      Name     :        String;
      Field    :    out Field_Type)
   is
      Child : constant Configuration'Class := Config.Child (Name);
      Value : constant String := Child.Children.Element (1).Config_Name;
   begin
      Field := From_String (Value);
   end Configure;

   -------------------------
   -- Configure_Container --
   -------------------------

   procedure Configure_Container
     (Config    :        Configuration;
      Add       : not null access procedure (Config : Configuration))
   is
      It    : Cursor := Config.First;
   begin
      while Has_Element (It) loop
         Add (Element (It));
         Next (It);
      end loop;
--        for Item of Config loop
--           Add (Item);
--        end loop;
   end Configure_Container;

   -------------------------
   -- Configure_Structure --
   -------------------------

   procedure Configure_Structure
     (Config    :        Configuration;
      Name      :        String;
      Structure :    out Structure_Type)
   is
      Child : constant Configuration := Config.Child (Name);
   begin
      Configure (Structure, Child);
   end Configure_Structure;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Configuration;
      Position  : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
      Item : constant Configuration_Access :=
               Configuration_Vector.Element (Position.Position);
   begin
      return (Element => Item);
   end Constant_Reference;

   --------------
   -- Contains --
   --------------

   function Contains (Config  : Configuration;
                      Name    : String)
                      return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in 1 .. Config.Children.Last_Index loop
         if Equal (Config.Children.Element (I).Name, +Name) then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Configuration is
   begin
      return Configuration_Vector.Element (Item.Position).all;
   end Element;

   -----------
   -- First --
   -----------

   function First (Item : Configuration) return Cursor is
   begin
      if Item.Children.Is_Empty then
         return No_Element;
      else
         return (Position => Item.Children.First);
      end if;
   end First;

   -----------
   -- First --
   -----------

   overriding function First (Object : Iterator) return Cursor is
   begin
      if Object.Current = No_Element then
         return Object.Container.First;
      else
         return Object.Current;
      end if;
   end First;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return String
   is
   begin
      return From_Config.Child (Field_Name).Children.Element (1).Config_Name;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : String)
                 return String
   is
   begin
      if From_Config.Contains (Field_Name) then
         if From_Config.Child (Field_Name).Child_Count = 0 then
            return "yes";
         else
            return From_Config.Get (Field_Name);
         end if;
      else
         return Default_Value;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return String
   is
   begin
      return From_Config.Children.Element (Field_Index).Config_Name;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return Boolean
   is
      Result : constant String := From_Config.Get (Field_Name, "no");
   begin
      return Result = "yes" or else Result = "true";
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Boolean)
                 return Boolean
   is
      Result : constant String := From_Config.Get (Field_Name, "");
   begin
      if Result = "" then
         return Default_Value;
      else
         return Result = "yes";
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return Integer
   is
      Result : constant String := From_Config.Get (Field_Name, "0");
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            return Integer'Value (Result (1 .. I - 1));
         end if;
      end loop;
      return Integer'Value (Result);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Integer)
                 return Integer
   is
   begin
      if From_Config.Contains (Field_Name)
        and then From_Config.Get (Field_Name) /= ""
      then
         return From_Config.Get (Field_Name);
      else
         return Default_Value;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Integer
   is
      Result : constant String := From_Config.Get (Field_Index);
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            return Integer'Value (Result (1 .. I - 1));
         end if;
      end loop;
      return Integer'Value (Result);
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "cannot convert field" & Field_Index'Image & " to an integer: "
           & Result;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return Float
   is
      use Ada.Strings, Ada.Strings.Fixed;
      Result      : constant String :=
                      Trim (From_Config.Get (Field_Name, "0.0"), Left);
      Start       : Positive := Result'First;
      Approximate : Boolean := False;
   begin
      for I in Result'Range loop
         if I = Result'First and then Result (I) = '~' then
            Approximate := True;
            Start := Start + 1;
         elsif Result (I) not in '0' .. '9' and then Result (I) /= '.'
           and then Result (I) /= 'e' and then Result (I) /= 'E'
           and then Result (I) /= '+' and then Result (I) /= '-'
         then
            declare
               X : Float :=
                     Float'Value (Result (Start .. I - 1));
            begin
               if Approximate then
                  X := X * 0.9
                    + Ada.Numerics.Float_Random.Random (Gen) * 0.2 * X;
               end if;

               if Result (I) = '%' then
                  return X / 100.0;
               elsif Result (I) in 'K' | 'k' then
                  return X * 1_000.0;
               elsif Result (I) in 'M' | 'm' then
                  return X * 1_000_000.0;
               elsif Result (I) in 'G' | 'g' then
                  return X * 1_000_000_000.0;
               else
                  return X;
               end if;
            end;
         end if;
      end loop;

      declare
         X : Float := Float'Value (Result (Start .. Result'Last));
      begin
         if Approximate then
            X := X * 0.9
              + Ada.Numerics.Float_Random.Random (Gen) * 0.2 * X;
         end if;
         return X;
      end;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Float)
                 return Float
   is
   begin
      if From_Config.Contains (Field_Name) then
         return Get (From_Config, Field_Name);
      else
         return Default_Value;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (From_Config : Configuration;
      Field_Name  : String)
      return Long_Float
   is
      use Ada.Strings, Ada.Strings.Fixed;
      Result : constant String :=
                 Trim (From_Config.Get (Field_Name, "0.0"), Left);
      Start       : Positive := Result'First;
      Approximate : Boolean := False;
   begin
      for I in Result'Range loop
         if I = Result'First and then Result (I) = '~' then
            Approximate := True;
            Start := Start + 1;
         elsif Result (I) not in '0' .. '9' and then Result (I) /= '.'
           and then Result (I) /= 'e' and then Result (I) /= 'E'
           and then Result (I) /= '+' and then Result (I) /= '-'
         then
            declare
               X : Long_Float :=
                     Long_Float'Value (Result (Start .. I - 1));
            begin
               if Approximate then
                  X := X * 0.9 +
                    Long_Float (Ada.Numerics.Float_Random.Random (Gen))
                      * 0.2 * X;
               end if;
               if Result (I) = '%' then
                  return X / 100.0;
               elsif Result (I) in 'K' | 'k' then
                  return X * 1_000.0;
               elsif Result (I) in 'M' | 'm' then
                  return X * 1_000_000.0;
               elsif Result (I) in 'G' | 'g' then
                  return X * 1_000_000_000.0;
               else
                  return X;
               end if;
            end;
         end if;
      end loop;
      declare
         X : Long_Float := Long_Float'Value (Result (Start .. Result'Last));
      begin
         if Approximate then
            X := X * 0.9 +
              Long_Float (Ada.Numerics.Float_Random.Random (Gen)) * 0.2 * X;
         end if;
         return X;
      end;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Long_Float)
                 return Long_Float
   is
   begin
      if From_Config.Contains (Field_Name) then
         return Get (From_Config, Field_Name);
      else
         return Default_Value;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Float
   is
      Field_Name : constant String := From_Config.Get (Field_Index);
   begin
      return Float'Value (Field_Name);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Long_Float
   is
      Field_Name : constant String := From_Config.Get (Field_Index);
   begin
      return Long_Float'Value (Field_Name);
   end Get;

   --------------
   -- Get_Enum --
   --------------

   function Get_Enum
     (Config   :        Configuration;
      Name     :        String)
      return Enum
   is
      Value : constant String := Config.Get (Name);
   begin
      return Enum'Value (Value);
   end Get_Enum;

   ---------------------------
   -- Get_Enum_With_Default --
   ---------------------------

   function Get_Enum_With_Default
     (Config   :        Configuration;
      Name     :        String)
      return Enum
   is
      Value : constant String := Config.Get (Name, "");
   begin
      if Value /= "" then
         return Enum'Value (Value);
      else
         return Default;
      end if;
   end Get_Enum_With_Default;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Configuration_Vector.Has_Element (Position.Position);
   end Has_Element;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Config   : Configuration;
      Child_Name : String;
      Process    : not null access procedure (Position : Cursor))
   is
      It : Cursor := Config.First;
   begin
      while Has_Element (It) loop
         if Element (It).Config_Name = Child_Name then
            Process (It);
         end if;
         Next (It);
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Configuration)
      return Configuration_Iterator_Interfaces.Reversible_Iterator'Class
   is

      V : constant Configuration_Access := Container'Unrestricted_Access;
      Result : constant Iterator :=
                 (Container => V, Current => No_Element);
   begin
      return Result;
   end Iterate;

   ------------------------
   -- Iterate_Attributes --
   ------------------------

   procedure Iterate_Attributes
     (Config  : Configuration;
      Process : not null access
        procedure (Name : String;
                   Value : String))
   is
   begin
      for Position in Config.Attributes.Iterate loop
         Process (Attribute_Maps.Key (Position),
                  Attribute_Maps.Element (Position));
      end loop;
   end Iterate_Attributes;

   ----------
   -- Last --
   ----------

   overriding function Last (Object : Iterator) return Cursor is
   begin
      if Object.Current = No_Element then
         return (Position => Object.Container.Children.Last);
      else
         return Object.Current;
      end if;
   end Last;

   ----------------
   -- New_Config --
   ----------------

   function New_Config (Name : String) return Configuration is
   begin
      return Configuration'
        (Name   => Ada.Strings.Unbounded.To_Unbounded_String (Name),
         others => <>);
   end New_Config;

   ----------------
   -- New_Config --
   ----------------

   function New_Config (Index : Integer) return Configuration is
   begin
      return New_Config
        (Ada.Strings.Fixed.Trim (Index'Image, Ada.Strings.Left));
   end New_Config;

   ----------
   -- Next --
   ----------

   procedure Next (Item : in out Cursor) is
   begin
      Configuration_Vector.Next (Item.Position);
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return (Position => Configuration_Vector.Next (Position.Position));
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return (Position => Configuration_Vector.Previous (Position.Position));
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Container : aliased in out Configuration;
      Position  : Cursor)
      return Reference_Type
   is
      pragma Unreferenced (Container);
      Item : constant Configuration_Access :=
               Configuration_Vector.Element (Position.Position);
   begin
      return (Element => Item);
   end Reference;

   --------------------
   -- Required_Child --
   --------------------

   function Required_Child
     (Of_Config  : Configuration;
      Child_Name : String)
      return Configuration
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in 1 .. Of_Config.Children.Last_Index loop
         declare
            Child_Config : constant Configuration_Access :=
                             Of_Config.Children.Element (I);
         begin
            if Equal (Child_Config.Name, +Child_Name) then
               return Of_Config.Children.Element (I).all;
            end if;
         end;
      end loop;
      raise Constraint_Error with
        "configuration " &
        Ada.Strings.Unbounded.To_String (Of_Config.Name) &
        " has no child named " & Child_Name;
   end Required_Child;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Config : in out Configuration;
      Name   : String;
      Value  : String)
   is
   begin
      if Config.Attributes.Contains (Name) then
         if Value = "" then
            Config.Attributes.Delete (Name);
         else
            Config.Attributes.Replace (Name, Value);
         end if;
      elsif Value /= "" then
         Config.Attributes.Insert (Name, Value);
      end if;
   end Set_Attribute;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path
     (Config         : in out Configuration;
      Path           : String;
      Value          : String;
      Path_Separator : Character := '.')
   is
      use Ada.Strings, Ada.Strings.Fixed;
      Sep_Index : constant Natural :=
                    Index (Path, (1 => Path_Separator));
   begin
      if Sep_Index = 0 then
         Config.Add (Path, Value);
      else
         declare
            Field : constant String := Path (Path'First .. Sep_Index - 1);
         begin
            if Config.Contains (Field) then
               declare
                  use Ada.Strings.Unbounded;
               begin
                  for Child of Config.Children loop
                     if Child.Name = Field then
                        Child.Set_Path
                          (Path (Sep_Index + 1 .. Path'Last),
                           Value, Path_Separator);
                        exit;
                     end if;
                  end loop;
               end;
            else
               declare
                  Child_Config : Configuration :=
                                   New_Config (Field);
               begin
                  Child_Config.Set_Path
                    (Path (Sep_Index + 1 .. Path'Last),
                     Value, Path_Separator);
                  Config.Add (Child_Config);
               end;
            end if;
         end;
      end if;
   end Set_Path;

   --------------------
   -- To_Float_Value --
   --------------------

   function To_Float_Value
     (Text : String)
      return Float
   is
      use Ada.Strings, Ada.Strings.Fixed;
      Result : constant String :=
                 Trim (Text, Both);
      Start       : Positive := Result'First;
      Approximate : Boolean := False;
   begin
      for I in Result'Range loop
         if I = Result'First and then Result (I) = '~' then
            Approximate := True;
            Start := Start + 1;
         elsif Result (I) not in '0' .. '9' and then Result (I) /= '.'
           and then Result (I) /= 'e' and then Result (I) /= 'E'
           and then Result (I) /= '+' and then Result (I) /= '-'
         then
            declare
               X : Float :=
                     Float'Value (Result (Start .. I - 1));
            begin
               if Approximate then
                  X := X * 0.9
                    + Ada.Numerics.Float_Random.Random (Gen) * 0.2 * X;
               end if;

               if Result (I) = '%' then
                  return X / 100.0;
               elsif Result (I) in 'K' | 'k' then
                  return X * 1_000.0;
               elsif Result (I) in 'M' | 'm' then
                  return X * 1_000_000.0;
               elsif Result (I) in 'G' | 'g' then
                  return X * 1_000_000_000.0;
               else
                  return X;
               end if;
            end;
         end if;
      end loop;

      declare
         X : Float := Float'Value (Result (Start .. Result'Last));
      begin
         if Approximate then
            X := X * 0.9
              + Ada.Numerics.Float_Random.Random (Gen) * 0.2 * X;
         end if;
         return X;
      end;
   end To_Float_Value;

   -----------
   -- Value --
   -----------

   function Value (Of_Config : Configuration)
                   return String
   is
   begin
      return Of_Config.Children.Element (1).Config_Name;
   end Value;

   -----------
   -- Value --
   -----------

   function Value
     (Of_Config : Configuration;
      Default_Value : Integer := 0)
      return Integer
   is
   begin
      if Of_Config.Children.Is_Empty then
         return Default_Value;
      else
         return Integer'Value (Of_Config.Value);
      end if;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Of_Config     : Configuration;
                   Default_Value : Float := 0.0)
                   return Float
   is
   begin
      if Of_Config.Children.Is_Empty then
         return Default_Value;
      else
         return To_Float_Value (Of_Config.Value);
      end if;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Of_Config     : Configuration;
                   Default_Value : Long_Float := 0.0)
                   return Long_Float
   is
   begin
      if Of_Config.Children.Is_Empty then
         return Default_Value;
      else
         return Long_Float (To_Float_Value (Of_Config.Value));
      end if;
   end Value;

end Tropos;
