
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Collections;
using System.Configuration;
using System.Web;
using System.Threading;
using System.Globalization;
using System.Text;
#endregion

public partial class view_cash_rcpt : System.Web.UI.Page
{
    string oldinv = "";
    string recid = "";
   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_cash_rcpt.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {

                if (Convert.ToString(Session["cash_receipt_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["cash_receipt_add_button"] = null;
                }
                if (Session["view_cash_receipt_cust_reckey_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["view_cash_receipt_cust_reckey_index"]);
                        Session["view_cash_receipt_cust_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_cash_receipt_cust_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }            
            

        } //  ! Page.IsPostBack

             


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {
           TextBox cust = (TextBox)FormView1.FindControl("custnoTextBox");
           Label custname = (Label)FormView1.FindControl("custnameLabel");
           TextBox memono = (TextBox)FormView1.FindControl("memonumTextBox");
           TextBox bnk = (TextBox)FormView1.FindControl("bnkcdTextBox");
           Label bnknam = (Label)FormView1.FindControl("bnknameLabel");
           TextBox notapp = (TextBox)FormView1.FindControl("notappTextBox");
           TextBox date = (TextBox)FormView1.FindControl("memodateTextBox");
           TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
           TextBox curr = (TextBox)FormView1.FindControl("currTextBox");
           TextBox exch = (TextBox)FormView1.FindControl("exchTextBox");
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");

           cust.Focus();
           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
          // try
           //{
               UserClass UserLogin = (UserClass)Session["User"];
               account con = new account();
               DataSet ds = new DataSet();
               ds = con.EnterEditCashReceipts("Addnew", "", UserLogin.UserName, "", "", 0, "", 0, "", "", "", 0, 0, "", "");

               cust.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
               custname.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
               memono.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
               date.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
               amt.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
               bnk.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
               bnknam.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
               curr.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
               exch.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
               notapp.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
               reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
               
              
           //}
           //catch { }
           
           FormView2.Visible = false;
           GridView1.Visible = false;
           AddNewFormView2Button.Visible = false;

       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           TextBox cust = (TextBox)FormView1.FindControl("custnoTextBox");           
           cust.Focus();
           FormView2.Visible = false;
           GridView1.Visible = false;
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {           
           
           try
           { 
               
               Label reckey = (Label)FormView1.FindControl("reckeyLabel");
               Session["cash_receipt_cust_reckey_rec"] = reckey.Text.Trim();
               
           }
           catch { }
           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }
   
   protected void delete_Button_Click(object sender, EventArgs e)
   {

       Label reckey = (Label)FormView1.FindControl("reckeyLabel");
       UserClass UserLogin = (UserClass)Session["User"];

       try
       {
           ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DataDelete";
           ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

           FormView1.ChangeMode(FormViewMode.ReadOnly);

           Response.Write("<script>window.location.href='view_cash_rcpt.aspx'</script>");
       }
       catch { }

   }

   protected void Formview1_InsertCancelButtonClick(object sender, EventArgs e)
   {
       try
       {
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
           UserClass UserLogin = (UserClass)Session["User"];
           account con = new account();
           DataSet ds = new DataSet();
           ds = con.EnterEditCashReceipts("DataDelete", "", UserLogin.UserName, "", "", 0, "", 0, "", "", "", 0, 0, "", reckey.Text.Trim());

       }
       catch { }

   }

   protected void UpdateButton_Click(object sender, EventArgs e)
   {
       TextBox cust = (TextBox)FormView1.FindControl("custnoTextBox");
       Label custname = (Label)FormView1.FindControl("custnameLabel");
       TextBox memono = (TextBox)FormView1.FindControl("memonumTextBox");
       TextBox bnk = (TextBox)FormView1.FindControl("bnkcdTextBox");
       Label bnknam = (Label)FormView1.FindControl("bnknameLabel");
       TextBox notapp = (TextBox)FormView1.FindControl("notappTextBox");
       TextBox date = (TextBox)FormView1.FindControl("memodateTextBox");
       TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
       TextBox curr = (TextBox)FormView1.FindControl("currTextBox");
       TextBox exch = (TextBox)FormView1.FindControl("exchTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
       
       if (memono.Text == "")
           memono.Text = "0";
       if (amt.Text == "")
           amt.Text = "0";

       UserClass UserLogin = (UserClass)Session["User"];
       
       account val = new account();

       bool check = val.validActWriteCrDbist("ValidateUpdate", "", UserLogin.UserName, cust.Text.Trim(), custname.Text.Trim(), Convert.ToInt32(memono.Text.Trim()), "", Convert.ToDecimal(amt.Text.Trim()), "", "", 0, "", "");

            string value = Convert.ToString(check);
            if (value == "True")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcustname"].DefaultValue = custname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmchkno"].DefaultValue = memono.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbnkcd"].DefaultValue = bnk.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbnknam"].DefaultValue = bnknam.Text.Trim();
                ObjectDataSource1.SelectParameters["prmchkdt"].DefaultValue = date.Text.Trim();
                ObjectDataSource1.SelectParameters["prmamt"].DefaultValue = amt.Text.Trim();
                ObjectDataSource1.SelectParameters["prmnotapp"].DefaultValue = notapp.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcur_cod"].DefaultValue = curr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmex_rate"].DefaultValue = exch.Text.Trim();


                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
       

   }

   protected void InsertButton_Click(object sender, EventArgs e)
   {
       TextBox cust = (TextBox)FormView1.FindControl("custnoTextBox");
       Label custname = (Label)FormView1.FindControl("custnameLabel");
       TextBox memono = (TextBox)FormView1.FindControl("memonumTextBox");
       TextBox bnk = (TextBox)FormView1.FindControl("bnkcdTextBox");
       Label bnknam = (Label)FormView1.FindControl("bnknameLabel");
       TextBox notapp = (TextBox)FormView1.FindControl("notappTextBox");
       TextBox date = (TextBox)FormView1.FindControl("memodateTextBox");
       TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
       TextBox curr = (TextBox)FormView1.FindControl("currTextBox");
       TextBox exch = (TextBox)FormView1.FindControl("exchTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");

       if (memono.Text == "")
           memono.Text = "0";
       if (amt.Text == "")
           amt.Text = "0";

       UserClass UserLogin = (UserClass)Session["User"];
       
       account val = new account();

       bool check = val.validActWriteCrDbist("ValidateAdd", "", UserLogin.UserName, cust.Text.Trim(), custname.Text.Trim(), Convert.ToInt32(memono.Text.Trim()), "", Convert.ToDecimal(amt.Text.Trim()), "", "", 0, "", "");

            string value = Convert.ToString(check);
            if (value == "True")
            {
                Session["cash_receipt_cust_reckey_rec"] = reckey.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcustname"].DefaultValue = custname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmchkno"].DefaultValue = memono.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbnkcd"].DefaultValue = bnk.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbnknam"].DefaultValue = bnknam.Text.Trim();
                ObjectDataSource1.SelectParameters["prmchkdt"].DefaultValue = date.Text.Trim();
                ObjectDataSource1.SelectParameters["prmamt"].DefaultValue = amt.Text.Trim();
                ObjectDataSource1.SelectParameters["prmnotapp"].DefaultValue = notapp.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcur_cod"].DefaultValue = curr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmex_rate"].DefaultValue = exch.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
                FormView2.ChangeMode(FormViewMode.Insert);

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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_cash_rcpt.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("entr_cshrcpt_list.aspx");
    }

    

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_cash_receipt_cust_reckey_index"] = GridView1.SelectedIndex;
        Session["view_cash_receipt_cust_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
        TextBox invdate = (TextBox)FormView2.FindControl("invdateLabel");
        TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
        TextBox disc = (TextBox)FormView2.FindControl("disTextBox");
        TextBox cshpy = (TextBox)FormView2.FindControl("disTextBox");
        TextBox totl = (TextBox)FormView2.FindControl("ttlappTextBox");
        TextBox balaf = (TextBox)FormView2.FindControl("disTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");
        

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (baldue.Text == "")
            baldue.Text = "0.00";
        if (totl.Text == "")
            totl.Text = "0.00";
        if (disc.Text == "")
            disc.Text = "0.00";
        UserClass UserLogin = (UserClass)Session["User"];
        
        account val = new account();

        bool check = val.validEnterEditCashReceiptView("ValidateUpdate", "", UserLogin.UserName, "", Convert.ToInt32(invno.Text.Trim()), invdate.Text.Trim(), Convert.ToDecimal(baldue.Text.Trim()), Convert.ToDecimal(disc.Text.Trim()), Convert.ToDecimal(cshpy.Text.Trim()), Convert.ToDecimal(totl.Text.Trim()), act.Text.Trim(), "", Convert.ToDecimal(balaf.Text.Trim()), Convert.ToString(Session["cash_receipt_cust_reckey_rec"]), Convert.ToString(Session["view_cash_receipt_cust_reckey"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prminvno"].DefaultValue = invno.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prminvdt"].DefaultValue = invdate.Text.Trim();
                ObjectDataSource3.SelectParameters["prmbaldu"].DefaultValue = baldue.Text.Trim();
                ObjectDataSource3.SelectParameters["prmdisc"].DefaultValue = disc.Text.Trim();
                ObjectDataSource3.SelectParameters["prmcashpy"].DefaultValue = cshpy.Text.Trim();                
                ObjectDataSource3.SelectParameters["prmtotl_app"].DefaultValue = totl.Text.Trim();
                ObjectDataSource3.SelectParameters["prmbalaftr"].DefaultValue = balaf.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactno"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactdscr"].DefaultValue = actdscr.Text.Trim();                
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_cash_rcpt.aspx'</script>");
            }
    }
    
    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

        TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
        TextBox invdate = (TextBox)FormView2.FindControl("invdateLabel");
        TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
        TextBox disc = (TextBox)FormView2.FindControl("disTextBox");
        TextBox cshpy = (TextBox)FormView2.FindControl("disTextBox");
        TextBox totl = (TextBox)FormView2.FindControl("ttlappTextBox");
        TextBox balaf = (TextBox)FormView2.FindControl("disTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (baldue.Text == "")
            baldue.Text = "0.00";
        if (totl.Text == "")
            totl.Text = "0.00";
        if (disc.Text == "")
            disc.Text = "0.00";
        UserClass UserLogin = (UserClass)Session["User"];

        account val = new account();

        bool check = val.validEnterEditCashReceiptView("ValidateAdd", "", UserLogin.UserName, "", Convert.ToInt32(invno.Text.Trim()), invdate.Text.Trim(), Convert.ToDecimal(baldue.Text.Trim()), Convert.ToDecimal(disc.Text.Trim()), Convert.ToDecimal(cshpy.Text.Trim()), Convert.ToDecimal(totl.Text.Trim()), act.Text.Trim(), "", Convert.ToDecimal(balaf.Text.Trim()), Convert.ToString(Session["cash_receipt_cust_reckey_rec"]), Convert.ToString(Session["view_cash_receipt_cust_reckey"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {

            Session["view_cash_receipt_cust_reckey"] = reckey.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prminvno"].DefaultValue = invno.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prminvdt"].DefaultValue = invdate.Text.Trim();
                ObjectDataSource3.SelectParameters["prmbaldu"].DefaultValue = baldue.Text.Trim();
                ObjectDataSource3.SelectParameters["prmdisc"].DefaultValue = disc.Text.Trim();
                ObjectDataSource3.SelectParameters["prmcashpy"].DefaultValue = cshpy.Text.Trim();
                ObjectDataSource3.SelectParameters["prmtotl_app"].DefaultValue = totl.Text.Trim();
                ObjectDataSource3.SelectParameters["prmbalaftr"].DefaultValue = balaf.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactno"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactdscr"].DefaultValue = actdscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_cash_rcpt.aspx'</script>");

            }
        

    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
            TextBox invdate = (TextBox)FormView2.FindControl("invdateLabel");
            TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
            TextBox disc = (TextBox)FormView2.FindControl("disTextBox");
            TextBox cshpy = (TextBox)FormView2.FindControl("disTextBox");
            TextBox totl = (TextBox)FormView2.FindControl("ttlappTextBox");
            TextBox balaf = (TextBox)FormView2.FindControl("disTextBox");
            TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
            TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");


            TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
            invno.Focus();

            

            UserClass UserLogin = (UserClass)Session["User"];
            try
            {
                account val = new account();
                DataSet ds = new DataSet();
                ds = val.EnterEditCashReceiptView("AddNew", "", UserLogin.UserName, "", 0, "", 0, 0, 0, 0, "", "", 0, Convert.ToString(Session["cash_receipt_cust_reckey_rec"]), "");

                
                    invno.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
                    invdate.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
                    baldue.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
                    disc.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
                    cshpy.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
                    totl.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
                    balaf.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
                    act.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
                    actdscr.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);                    

                    reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
                    GridView1.Visible = false;
                
            }
                catch { }

        }
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
            invno.Focus();
            GridView1.Visible = false;
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                Session["view_cash_receipt_cust_reckey"] = reckey.Text.Trim();

            }
            catch { }
            GridView1.Visible = true;
            
            try
            {
                if (FormView2.DataItemCount.ToString() == "0")
                    AddNewFormView2Button.Visible = true;
                else
                    AddNewFormView2Button.Visible = false;
            }
            catch { }
        }
    }
    protected void CancelButton_FormView2_Delete(object sender, EventArgs e)
    {
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            account con = new account();
            DataSet ds = new DataSet();
            ds = con.EnterEditCashReceiptView("AddDelete", "", UserLogin.UserName, "", 0, "", 0, 0, 0, 0, "", "", 0, "", reckey.Text.Trim());
        }
        catch { }
        FormView1.Visible = true;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='view_cash_rcpt.aspx'</script>");
    }
    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {
       
        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");

        
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "DataDelete";
                ObjectDataSource3.SelectParameters["prmOut"].DefaultValue = Convert.ToString(Session["cash_receipt_cust_reckey_rec"]);
                Session["view_cash_receipt_cust_reckey_index"] = null;
                Response.Write("<script>window.location.href='view_cash_rcpt.aspx'</script>");

          

    }

    protected void AddNewFormView2Button_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
        AddNewFormView2Button.Visible = false;
    }

    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);

    }

    protected void Invoice_OnTextChange(object sender, EventArgs e)
    {
        TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
        TextBox invdate = (TextBox)FormView2.FindControl("invdateLabel");
        TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
        TextBox disc = (TextBox)FormView2.FindControl("disTextBox");
        TextBox cshpy = (TextBox)FormView2.FindControl("disTextBox");
        TextBox totl = (TextBox)FormView2.FindControl("ttlappTextBox");
        TextBox balaf = (TextBox)FormView2.FindControl("disTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");

        Label cust = (Label)FormView1.FindControl("custnoLabel");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            account con = new account();
            DataSet ds = new DataSet();
            ds = con.CrDbInvoiceLookup("Search", UserLogin.UserName, "inv", "EQUAL", invno.Text.Trim(), cust.Text.Trim());


            //sqft.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
            invdate.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
            baldue.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);


            disc.Focus();
        }
        catch { }

    }
    

}
