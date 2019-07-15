using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;
using System.Text;
using System.Data.OleDb;
using System.IO;
using System.Data.Common;

public partial class view_item_popup : System.Web.UI.Page
{  
    protected void Page_PreRender(object sender, EventArgs e)
    {        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];                

        Int32 i = Convert.ToInt32(Request.QueryString["line"]);
        Int32 line_total = Convert.ToInt32(Request.QueryString["totline"]);        
        string orderno = Convert.ToString(Session["order_est"]);     

        if (i > line_total)
        {
            Order mail = new Order();
            mail.OrderMail(UserLogin.UserName, "MailOrder", orderno, "001", "");
            Response.Write("<script>self.window.close();</script>");            
        }
        
    }  

    protected void Page_Load(object sender, EventArgs e)
    {        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];       

        if (Convert.ToString(Session["itm_visibility"]) == "emode")
            FormView1.ChangeMode(FormViewMode.Edit);

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_item_popup.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //lblUser.Text = UserLogin.UserName;
            //labelcompany.Text = PrmComp;
            if (aUsers == "external")
            {
            }
            //if (vCanRun == false)
            //{
            //    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
            //    Response.Write("<script>window.location.href = 'login.aspx';</script>");
            //}
        }

       
        if (!Page.IsPostBack)
        {            
            
        }
        
    }
        

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }
    

    protected void GridView1_RowCreated(object sender, GridViewRowEventArgs e)
    {
       
    }
    protected void GridView1_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
           
    }
           

  

    

    protected void grid_view1_PreRender(object sender, EventArgs e)
    {
     
    }


    protected void add_button_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }


   


    protected void custpart_Click(object sender, EventArgs e)
    {
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
            TextBox dscr = (TextBox)FormView1.FindControl("DscrTextBox");

            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.ItemPartLookup(Convert.ToString(Session["order_entry_cust_no"]), "search", UserLogin.UserName, custpart.Text);
            custpart.Text = ds.Tables[0].Rows[0][0].ToString();
        }
        catch
        {
            //TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
            //custpart.Text = "";
        }        
    }


    protected void Quantity_Change_Click(object sender, EventArgs e)
    {
        try
        {
            TextBox quote = (TextBox)FormView1.FindControl("vQnoTextBox");
            TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");

            TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
            TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
            TextBox cascount = (TextBox)FormView1.FindControl("counterTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("VcasUnitTextBox");

            TextBox total = (TextBox)FormView1.FindControl("extpriceTextBox");
            TextBox qty = (TextBox)FormView1.FindControl("quantityTextBox");
            TextBox disc = (TextBox)FormView1.FindControl("discountTextBox");
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            if (qty.Text == "")
                qty.Text = "0";

            //if (quote.Text == "" || quote.Text.Trim() == "0")
            //{
            if (quote.Text.Trim() == "")
                quote.Text = "0";

            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.SelectfgLookup("search", UserLogin.UserName, "viewitemfg", "", fgitem.Text, Convert.ToString(Session["order_entry_cust_no"]), Convert.ToInt32(quote.Text.Trim()), Convert.ToInt32(qty.Text));

            price.Text = ds.Tables[0].Rows[0][10].ToString();
            Hiddenprice.Value = ds.Tables[0].Rows[0][10].ToString();



            decimal pprice = Convert.ToDecimal(ds.Tables[0].Rows[0][10].ToString());
            int qqty = Convert.ToInt32(qty.Text);
            int ccascount = Convert.ToInt32(cascount.Text);
            decimal discount = Convert.ToInt32(disc.Text);
            decimal caldis = 0;

            if (uom.Text == "M")
            {
                total.Text = Convert.ToString((pprice * qqty) / 1000);
                if (discount >= 1)
                {
                    caldis = (Convert.ToInt32(total.Text) * discount / 100);
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) - caldis);
                }
                Hiddentotal.Value = total.Text;
            }
            else if (uom.Text == "C")
            {
                total.Text = Convert.ToString(pprice * qqty / 100);
                if (discount >= 1)
                {
                    caldis = (Convert.ToInt32(total.Text) * discount / 100);
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) - caldis);
                }
                Hiddentotal.Value = total.Text;
            }
            else if (uom.Text == "CS" && ccascount != 0)
            {
                total.Text = Convert.ToString(pprice * qqty / ccascount);
                if (discount >= 1)
                {
                    caldis = (Convert.ToInt32(total.Text) * discount / 100);
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) - caldis);
                }
                Hiddentotal.Value = total.Text;
            }
            else
            {
                total.Text = Convert.ToString(pprice * qqty);
                if (discount >= 1)
                {
                    caldis = (Convert.ToInt32(total.Text) * discount / 100);
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) - caldis);
                }
                Hiddentotal.Value = total.Text;
            }
            // }

        }
        catch { }
    }


    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];    
        TextBox quantity = (TextBox)FormView1.FindControl("quantityTextBox");
        CheckBox minv = (CheckBox)FormView1.FindControl("vManagCheckBox");     
        DropDownList original = (DropDownList)FormView1.FindControl("DropDownList1");
        TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
        TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
        CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");
        TextBox itemname = (TextBox)FormView1.FindControl("Name1TextBox");
        TextBox totprice = (TextBox)FormView1.FindControl("extpriceTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
        TextBox qtyunit = (TextBox)FormView1.FindControl("counterTextBox");
        TextBox description = (TextBox)FormView1.FindControl("DscrTextBox");
        TextBox units = (TextBox)FormView1.FindControl("VcasUnitTextBox");
        TextBox description2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
        TextBox custpo = (TextBox)FormView1.FindControl("custpoTextBox");
        TextBox line = (TextBox)FormView1.FindControl("vLineTextBox");     
        DropDownList priority = (DropDownList)FormView1.FindControl("DropDownList2");
        DropDownList priority2 = (DropDownList)FormView1.FindControl("DropDownList3");
        TextBox duedate = (TextBox)FormView1.FindControl("requestdateTextBox");
        TextBox boardpo = (TextBox)FormView1.FindControl("vPonoTextBox");
        TextBox boardvend = (TextBox)FormView1.FindControl("vBoardVenTextBox");

        TextBox promisedate = (TextBox)FormView1.FindControl("promisdateTextBox");
        TextBox sman = (TextBox)FormView1.FindControl("vSmanTextBox");
        TextBox sman2 = (TextBox)FormView1.FindControl("vSman2TextBox");
        TextBox sman3 = (TextBox)FormView1.FindControl("vSman3TextBox");
        TextBox over = (TextBox)FormView1.FindControl("vOverTextBox");
        TextBox under = (TextBox)FormView1.FindControl("vUnderTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("vSnameTextBox");
        TextBox sname2 = (TextBox)FormView1.FindControl("vSname2TextBox");
        TextBox sname3 = (TextBox)FormView1.FindControl("vSname3TextBox");

        TextBox Cost = (TextBox)FormView1.FindControl("CostTextBox");
        TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");

        if (minv.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (tax.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        

        if (quantity.Text == "")
            quantity.Text = "0";
        if (price.Text == "")
            price.Text = "0";
        if (partial.Text == "")
            partial.Text = "0";
        if (discount.Text == "")
            discount.Text = "0";

        if (qtyunit.Text == "")
            qtyunit.Text = "0";
        if (units.Text == "")
            units.Text = "0";

        if (line.Text == "")
            line.Text = "0";
        if (boardpo.Text == "")
            boardpo.Text = "0";
        if (boardvend.Text == "")
            boardvend.Text = "0";
        if (over.Text == "")
            over.Text = "0";

        if (under.Text == "")
            under.Text = "0";
        if (Hiddenspct.Value == "")
            Hiddenspct.Value = "0";
        if (Hiddencomm.Value == "")
            Hiddencomm.Value = "0";
        if (Cost.Text == "")
        {
            Cost.Text = "0";
        }

        orderentry ord = new orderentry();
        ord.SelectViewItemEstimate(UserLogin.UserName, "Update", Convert.ToInt32(Session["order_est"]), Convert.ToInt32(Request.QueryString["line"]), "", fgitem.Text, custpart.Text, Convert.ToDecimal(quantity.Text.Trim()), itemname.Text, description.Text, description2.Text, "", Convert.ToDecimal(price.Text.Trim()), uom.Text, HiddenField2.Value, custpo.Text, "", 0, Convert.ToDecimal(discount.Text), priority2.Text, Convert.ToDateTime(duedate.Text), 0, priority.SelectedValue, Convert.ToDateTime("02/02/2010"), 0, Convert.ToInt32(qtyunit.Text), Convert.ToDecimal(partial.Text), Convert.ToInt32(units.Text), Convert.ToInt32(line.Text), Convert.ToInt32(boardpo.Text), sman.Text, sman2.Text, sman3.Text, original.SelectedValue, Convert.ToDecimal(over.Text), Convert.ToDecimal(under.Text), boardvend.Text, HiddenField1.Value, "", sname.Text, sname2.Text, sname3.Text, Convert.ToDecimal(Hiddenspct.Value), 0, 0, Convert.ToDecimal(Hiddencomm.Value), 0, 0, Convert.ToDecimal(Cost.Text), 0, "y");

        
        
        Int32 i = Convert.ToInt32(Request.QueryString["line"]) + 1;
        Int32 line_total = Convert.ToInt32(Request.QueryString["totline"]);
        Label orderno = (Label)FormView1.FindControl("ord_label");
        try
        {
            while (i <= (line_total + 1))
            {                
                Response.Write("<script>self.location.href='view_item_popup.aspx?line=" + i + "&totline=" + line_total + "';</script>");
                i++;
            }
                        
            Session["view_item_mode"] = null;
        }
        catch { }
               
    }



    protected void fgitem_value_Click(object sender, EventArgs e)
    {
        try
        {
            TextBox quote = (TextBox)FormView1.FindControl("vQnoTextBox");
            TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
            TextBox iname = (TextBox)FormView1.FindControl("Name1TextBox");
            TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
            TextBox dscr = (TextBox)FormView1.FindControl("DscrTextBox");
            TextBox dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
            TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
            TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
            TextBox cascount = (TextBox)FormView1.FindControl("counterTextBox");
            TextBox casunit = (TextBox)FormView1.FindControl("VcasUnitTextBox");
            TextBox cost = (TextBox)FormView1.FindControl("CostTextBox");
            DropDownList type = (DropDownList)FormView1.FindControl("DropDownList1");
            TextBox total = (TextBox)FormView1.FindControl("extpriceTextBox");
            TextBox qty = (TextBox)FormView1.FindControl("quantityTextBox");
            TextBox disc = (TextBox)FormView1.FindControl("discountTextBox");

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            if (qty.Text == "")
                qty.Text = "0";
            if (quote.Text.Trim() == "")
                quote.Text = "0";

            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.SelectfgLookup("search", UserLogin.UserName, "viewitemfg", "", fgitem.Text, Convert.ToString(Session["order_entry_cust_no"]), Convert.ToInt32(quote.Text.Trim()), Convert.ToInt32(qty.Text));            
            HiddenDropdown1.Value = ds.Tables[0].Rows[0][16].ToString();       
            fgitem.Text = ds.Tables[0].Rows[0][0].ToString().Replace(":", "\"");
            iname.Text = ds.Tables[0].Rows[0][1].ToString().Replace(":", "\"");
            HiddenFieldname.Value = ds.Tables[0].Rows[0][1].ToString().Replace(":", "\"");
            custpart.Text = ds.Tables[0].Rows[0][4].ToString().Replace(":", "\"");
            dscr.Text = ds.Tables[0].Rows[0][8].ToString().Replace(":", "\"");
            Hiddencustdesc.Value = ds.Tables[0].Rows[0][8].ToString().Replace(":", "\"");
            dscr2.Text = ds.Tables[0].Rows[0][9].ToString().Replace(":", "\"");
            Hiddencustdesc2.Value = ds.Tables[0].Rows[0][9].ToString().Replace(":", "\"");
            price.Text = ds.Tables[0].Rows[0][10].ToString();
            Hiddenprice.Value = ds.Tables[0].Rows[0][10].ToString();
            uom.Text = ds.Tables[0].Rows[0][11].ToString();
            HiddenTextBox1.Value = ds.Tables[0].Rows[0][11].ToString();
            cascount.Text = ds.Tables[0].Rows[0][12].ToString();
            Hiddencounter.Value = ds.Tables[0].Rows[0][12].ToString();
            casunit.Text = ds.Tables[0].Rows[0][13].ToString();
            Hiddencasunit.Value = ds.Tables[0].Rows[0][13].ToString();
            cost.Text = ds.Tables[0].Rows[0][15].ToString();
            Hiddencost.Value = ds.Tables[0].Rows[0][15].ToString();
            disc.Text = ds.Tables[0].Rows[0][17].ToString();
            Hiddendiscount.Value = ds.Tables[0].Rows[0][17].ToString();
            decimal pprice = Convert.ToDecimal(ds.Tables[0].Rows[0][10].ToString());
            int qqty = Convert.ToInt32(qty.Text);
            int ccascount = Convert.ToInt32(ds.Tables[0].Rows[0][12].ToString());
            decimal discount = Convert.ToInt32(ds.Tables[0].Rows[0][17].ToString());

            if (ds.Tables[0].Rows[0][11].ToString() == "M")
            {
                total.Text = Convert.ToString((pprice * qqty) / 1000);
                if (discount >= 1)
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                Hiddentotal.Value = total.Text;
            }
            else if (ds.Tables[0].Rows[0][11].ToString() == "C")
            {
                total.Text = Convert.ToString(pprice * qqty / 100);
                if (discount >= 1)
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                Hiddentotal.Value = total.Text;
            }
            else if (ds.Tables[0].Rows[0][11].ToString() == "CS" && ccascount != 0)
            {
                total.Text = Convert.ToString(pprice * qqty / ccascount);
                if (discount >= 1)
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                Hiddentotal.Value = total.Text;
            }
            else
            {
                total.Text = Convert.ToString(pprice * qqty);
                if (discount >= 1)
                    total.Text = Convert.ToString(Convert.ToInt32(total.Text) * discount / 100);
                Hiddentotal.Value = total.Text;
            }

         
            if (ds.Tables[0].Rows[0][16].ToString() == "O")
                type.SelectedIndex = 0;
            if (ds.Tables[0].Rows[0][16].ToString() == "C")
                type.SelectedIndex = 1;
            if (ds.Tables[0].Rows[0][16].ToString() == "N")
                type.SelectedIndex = 2;
            if (ds.Tables[0].Rows[0][16].ToString() == "Q")
                type.SelectedIndex = 3;
            if (ds.Tables[0].Rows[0][16].ToString() == "R")
                type.SelectedIndex = 4;
            if (ds.Tables[0].Rows[0][16].ToString() == "T")
                type.SelectedIndex = 5;
            if (ds.Tables[0].Rows[0][16].ToString() == "X")
                type.SelectedIndex = 6;    
        }
        catch { }
    }

    protected void InsertCancelButton_Click(object sender, EventArgs e)
    {
        Session["view_item_mode"] = null;
        HiddenQuoteNum.Value = "";
    }

   
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            

            if (FormView1.CurrentMode == FormViewMode.Edit)
            {


                TextBox vType = (TextBox)FormView1.FindControl("vTypeTextBox");
                vType.Text = "EA";

                Label ordLabel = (Label)FormView1.FindControl("ord_label");
                Label CustLabel = (Label)FormView1.FindControl("cust_label");
                ordLabel.Text = Convert.ToString(Session["order_est"]);
                CustLabel.Text = Convert.ToString(Session["order_entry_cust_no"]);
                TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
                custpart.Focus();
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_item_estimate.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                    TextBox sman1 = (TextBox)FormView1.FindControl("vSmanTextBox");
                    Hiddensman.Value = sman1.Text;
                    TextBox sman2 = (TextBox)FormView1.FindControl("vSman2TextBox");
                    TextBox sman3 = (TextBox)FormView1.FindControl("vSman3TextBox");

                    TextBox sname1 = (TextBox)FormView1.FindControl("vSnameTextBox");
                    Hiddensname.Value = sname1.Text;
                    TextBox sname2 = (TextBox)FormView1.FindControl("vSname2TextBox");
                    TextBox sname3 = (TextBox)FormView1.FindControl("vSname3TextBox");

                    TextBox overrun = (TextBox)FormView1.FindControl("vOverTextBox");
                    TextBox underrun = (TextBox)FormView1.FindControl("vUnderTextBox");
                    TextBox duedate = (TextBox)FormView1.FindControl("requestdateTextBox");
                    TextBox promisedate = (TextBox)FormView1.FindControl("promisdateTextBox");
                    TextBox custpo = (TextBox)FormView1.FindControl("custpoTextBox");
                    DropDownList priority = (DropDownList)FormView1.FindControl("DropDownList2");
                    DropDownList priority2 = (DropDownList)FormView1.FindControl("DropDownList3");
                    DropDownList type = (DropDownList)FormView1.FindControl("DropDownList1");
                    HiddenDropdown1.Value = type.Text;
                    promisedate.Enabled = false;
                    //TextBox spct1 = (TextBox)FormView1.FindControl("vSpctTextBox");
                    //TextBox spct2 = (TextBox)FormView1.FindControl("vSpct2TextBox");
                    //TextBox spct3 = (TextBox)FormView1.FindControl("vSpct3TextBox");
                    //TextBox comm1 = (TextBox)FormView1.FindControl("vScommTextBox");
                    //TextBox comm2 = (TextBox)FormView1.FindControl("vScomm2TextBox");
                    //TextBox comm3 = (TextBox)FormView1.FindControl("vScomm3TextBox");

                    CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");
                    Image imagepro = (Image)FormView1.FindControl("Image1");
                    imagepro.Visible = false;

                    if (aUsers == "external")
                    {
                        CheckBox minvch = (CheckBox)FormView1.FindControl("vManagCheckBox");
                        DropDownList rep = (DropDownList)FormView1.FindControl("DropDownList1");
                        TextBox bpotext = (TextBox)FormView1.FindControl("vPonoTextBox");
                        TextBox bvendtext = (TextBox)FormView1.FindControl("vBoardVenTextBox");
                        Label boardpolabel = (Label)FormView1.FindControl("bpolabel");
                        Label replabel = (Label)FormView1.FindControl("orlabel");
                        Label boardvendlabel = (Label)FormView1.FindControl("bvendlabel");
                        Label minvlabel = (Label)FormView1.FindControl("Manlabel");

                        TextBox itemname = (TextBox)FormView1.FindControl("Name1TextBox");
                        HiddenFieldname.Value = itemname.Text;
                        TextBox dscr1 = (TextBox)FormView1.FindControl("DscrTextBox");
                        Hiddencustdesc.Value = dscr1.Text;
                        TextBox dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
                        Hiddencustdesc2.Value = dscr2.Text;
                        //TextBox sman1 = (TextBox)FormView1.FindControl("vSmanTextBox");
                        //TextBox sman2 = (TextBox)FormView1.FindControl("vSman2TextBox");
                        //TextBox sman3 = (TextBox)FormView1.FindControl("vSman3TextBox");
                        TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
                        Hiddenprice.Value = price.Text;
                        TextBox uom = (TextBox)FormView1.FindControl("TextBox1");
                        HiddenTextBox1.Value = uom.Text;
                        //CheckBox tax = (CheckBox)FormView1.FindControl("taxableCheckBox");
                        TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
                        Hiddendiscount.Value = discount.Text;
                        TextBox qty = (TextBox)FormView1.FindControl("counterTextBox");
                        Hiddencounter.Value = qty.Text;
                        TextBox tprice = (TextBox)FormView1.FindControl("extpriceTextBox");
                        Hiddentotal.Value = tprice.Text;
                        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
                        TextBox units = (TextBox)FormView1.FindControl("VcasUnitTextBox");
                        Hiddencasunit.Value = units.Text;
                        TextBox prodate = (TextBox)FormView1.FindControl("promisdateTextBox");
                        TextBox typek = (TextBox)FormView1.FindControl("vTypeTextBox");
                        TextBox cost = (TextBox)FormView1.FindControl("CostTextBox");
                        TextBox line = (TextBox)FormView1.FindControl("vLineTextBox");

                        Image imapo = (Image)FormView1.FindControl("Image3");
                        Image image1 = (Image)FormView1.FindControl("Image12");
                        Image image2 = (Image)FormView1.FindControl("Image2");
                        Image image3 = (Image)FormView1.FindControl("Image7");
                        Image image4 = (Image)FormView1.FindControl("Image4");

                        Label cost1 = (Label)FormView1.FindControl("cost1Label");
                        TextBox cost2 = (TextBox)FormView1.FindControl("CostTextBox");
                        Hiddencost.Value = cost2.Text;
                        //Label uom1 = (Label)FormView1.FindControl("uomLabel2");
                        TextBox uom2 = (TextBox)FormView1.FindControl("TextBox1");
                        Label partlabel = (Label)FormView1.FindControl("partLabel");
                        Label unitlabel = (Label)FormView1.FindControl("qtyunitLabel");
                        Label unitpalletlabel = (Label)FormView1.FindControl("unitpalletLabel");

                        //uom1.Visible = false;
                        uom2.Enabled = false;
                        cost1.Visible = false;
                        cost2.Visible = false;
                        partlabel.Visible = false;
                        unitpalletlabel.Visible = false;
                        itemname.Enabled = false;
                        dscr1.Enabled = false;
                        dscr2.Enabled = false;
                        sman1.Enabled = false;
                        sman2.Enabled = false;
                        sman3.Enabled = false;
                        sname1.Enabled = false;
                        sname2.Enabled = false;
                        sname3.Enabled = false;
                        //spct1.Enabled = false;
                        //spct2.Enabled = false;
                        //spct3.Enabled = false;
                        //comm1.Enabled = false;
                        //comm2.Enabled = false;
                        //comm3.Enabled = false;
                        price.Enabled = false;
                        //uom.Enabled = false;
                        tax.Enabled = false;
                        discount.Enabled = false;
                        qty.Enabled = false;
                        tprice.Enabled = false;
                        partial.Visible = false;
                        units.Visible = false;
                        //priority.Enabled = false;
                        priority2.Enabled = false;
                        prodate.Enabled = false;

                        //typek.Enabled = false;
                        //cost.Enabled = false;
                        //line.Enabled = false;

                        minvch.Visible = false;
                        rep.Visible = false;
                        bpotext.Visible = false;
                        bvendtext.Visible = false;
                        boardpolabel.Visible = false;
                        replabel.Visible = false;
                        boardvendlabel.Visible = false;
                        minvlabel.Visible = false;
                        //overrun.Enabled = false;
                        //underrun.Enabled = false;

                        imapo.Visible = false;
                        image1.Visible = false;
                        image2.Visible = false;
                        image3.Visible = false;
                        image4.Visible = false;
                    }
                }
            }
        }
        catch
        {
            return;
        }

    }

    protected void FormView1_Unload(object sender, EventArgs e)
    {

        try
        {
            TextBox quote = (TextBox)FormView1.FindControl("vQnoTextBox");
            Label job = (Label)FormView1.FindControl("job_noTextBox");
            Label job2 = (Label)FormView1.FindControl("Jobno2Label");
            Label entryline = (Label)FormView1.FindControl("vlineLabe");
            TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
            Label estnote = (Label)FormView1.FindControl("EstNumLabel");
            Label reckey = (Label)FormView1.FindControl("Label_rec_key");

            Session["view_line_est"] = entryline.Text;
            Session["line"] = entryline.Text;
            Session["item"] = fgitem.Text;
            Session["order_rec_key"] = reckey.Text;
            Session["order_entry_est_no"] = estnote.Text;
            //Session["order_entry_quote_no"] = quote.Text.Trim();
            //Session["order_entry_add_job"] = job.Text.Trim();
            //Session["order_entry_add_job2"] = job2.Text.Trim();
        }
        catch { }
    }

    protected void newAddButton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        //NewAddButton.Visible = false;
    }

       
    protected void quote_text_changed(object sender, EventArgs e)
    {
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox quote1 = (TextBox)FormView1.FindControl("vQnoTextBox");
            TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
            TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
            TextBox itemname = (TextBox)FormView1.FindControl("Name1TextBox");
            TextBox dscr1 = (TextBox)FormView1.FindControl("DscrTextBox");
            TextBox dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
            TextBox qty = (TextBox)FormView1.FindControl("quantityTextBox");
            TextBox qtyunit = (TextBox)FormView1.FindControl("counterTextBox");
            TextBox price = (TextBox)FormView1.FindControl("priceTextBox");
            TextBox disc = (TextBox)FormView1.FindControl("discountTextBox");
            TextBox tprice = (TextBox)FormView1.FindControl("extpriceTextBox");
            TextBox uom = (TextBox)FormView1.FindControl("TextBox1");

            if (quote1.Text == "")
                quote1.Text = "0";
            if (HiddenQuoteNum.Value == "")
            {
                HiddenQuoteNum.Value = "0";
            }

                      
        }
        catch
        {
            TextBox quote1 = (TextBox)FormView1.FindControl("vQnoTextBox");

            quote1.Focus();

        }
    }


    protected void qtytextchange(object sender, EventArgs e)
    {        

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox quantity = (TextBox)FormView1.FindControl("quantityTextBox");
        TextBox custpart = (TextBox)FormView1.FindControl("CustPartTextBox");
        TextBox Cost = (TextBox)FormView1.FindControl("CostTextBox");
        Label EstimatNo = (Label)FormView1.FindControl("EstNumLabel");
        if (quantity.Text == "")
            quantity.Text = "0";
        orderentry ordentry = new orderentry();
        try
        {
            DataSet dsGetCost = new DataSet();
            dsGetCost = ordentry.SelectGetCost(UserLogin.UserName, "GetCost", Convert.ToDecimal(quantity.Text.Trim()), EstimatNo.Text.Trim(), custpart.Text.Trim());

            quantity.Text = dsGetCost.Tables[0].Rows[0][0].ToString();
            Cost.Text = dsGetCost.Tables[0].Rows[0][1].ToString();
        }
        catch { }
         
    }

}
