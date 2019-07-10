
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class company_view : System.Web.UI.Page
{
    

    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "company_list.aspx";
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

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;

                if (Convert.ToString(Session["company_list_addnew_rec"]) == "Add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["company_list_addnew_rec"] = null;
                }

            }
        }
        
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox company = (TextBox)FormView1.FindControl("companyTextBox");
            company.Focus();
            TextBox fid = (TextBox)FormView1.FindControl("fidTextBox");
            TextBox name = (TextBox)FormView1.FindControl("vnameTextBox");
            TextBox statid = (TextBox)FormView1.FindControl("sidTextBox");
            TextBox add1 = (TextBox)FormView1.FindControl("addr1TextBox");
            TextBox add2 = (TextBox)FormView1.FindControl("addr2TextBox");
            TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
            TextBox stat = (TextBox)FormView1.FindControl("stateTextBox");
            TextBox zip = (TextBox)FormView1.FindControl("zipTextBox");
            TextBox accomp = (TextBox)FormView1.FindControl("co_accTextBox");
            TextBox numper = (TextBox)FormView1.FindControl("num_perTextBox");
            TextBox acclevel = (TextBox)FormView1.FindControl("acc_levelTextBox");
            TextBox acc_dig1 = (TextBox)FormView1.FindControl("acc_dig1TextBox");
            TextBox acc_dig2 = (TextBox)FormView1.FindControl("acc_dig2TextBox");
            TextBox acc_dig3 = (TextBox)FormView1.FindControl("acc_dig3TextBox");
            TextBox acc_dig4 = (TextBox)FormView1.FindControl("acc_dig4TextBox");
            TextBox acc_dig5 = (TextBox)FormView1.FindControl("acc_dig5TextBox");
            TextBox yendoff = (TextBox)FormView1.FindControl("yend_offTextBox");
            TextBox firstyear = (TextBox)FormView1.FindControl("firstyearTextBox");
            TextBox prddt1 = (TextBox)FormView1.FindControl("prddt1TextBox");
            TextBox prddt2 = (TextBox)FormView1.FindControl("prddt2TextBox");
            TextBox currcode = (TextBox)FormView1.FindControl("curr_codeTextBox");
            TextBox prdnum = (TextBox)FormView1.FindControl("prdnumTextBox");
            numper.Text = "12";
            firstyear.Text = "2007";
            prdnum.Text = "1";
            prddt1.Text = "04/01/2006";
            prddt2.Text = "04/30/2006";
            

                      
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox fid = (TextBox)FormView1.FindControl("fidTextBox");
            fid.Focus();
    
        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView1.FindControl("reckeyLabel");
                Session["company_list_reckey_name"] = reckey.Text;
                Label company = (Label)FormView1.FindControl("companyLabel");
                Session["company_list_company"] = company.Text.Trim();
            }
            catch { }

        }
    }
    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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


    
    
    protected void lnk_Listcompany_Click(object sender, EventArgs e)
    {
        Response.Redirect("company_list.aspx");
    }
    protected void lnk_viewcompany_Click(object sender, EventArgs e)
    {
        Response.Redirect("company_view.aspx");
    }

    protected void addButton_Click(object sender, EventArgs e)
    {

        TextBox company = (TextBox)FormView1.FindControl("companyTextBox");
        TextBox fid = (TextBox)FormView1.FindControl("fidTextBox");
        TextBox name = (TextBox)FormView1.FindControl("vnameTextBox");
        TextBox statid = (TextBox)FormView1.FindControl("sidTextBox");
        TextBox add1 = (TextBox)FormView1.FindControl("addr1TextBox");
        TextBox add2 = (TextBox)FormView1.FindControl("addr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
        TextBox stat = (TextBox)FormView1.FindControl("stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("zipTextBox");
        TextBox accomp = (TextBox)FormView1.FindControl("co_accTextBox");
        TextBox numper = (TextBox)FormView1.FindControl("num_perTextBox");
        TextBox acclevel = (TextBox)FormView1.FindControl("acc_levelTextBox");
        TextBox acc_dig1 = (TextBox)FormView1.FindControl("acc_dig1TextBox");
        TextBox acc_dig2 = (TextBox)FormView1.FindControl("acc_dig2TextBox");
        TextBox acc_dig3 = (TextBox)FormView1.FindControl("acc_dig3TextBox");
        TextBox acc_dig4 = (TextBox)FormView1.FindControl("acc_dig4TextBox");
        TextBox acc_dig5 = (TextBox)FormView1.FindControl("acc_dig5TextBox");
        TextBox yendoff = (TextBox)FormView1.FindControl("yend_offTextBox");
        TextBox firstyear = (TextBox)FormView1.FindControl("firstyearTextBox");
        TextBox prddt1 = (TextBox)FormView1.FindControl("prddt1TextBox");
        TextBox prddt2 = (TextBox)FormView1.FindControl("prddt2TextBox");
        TextBox currcode = (TextBox)FormView1.FindControl("curr_codeTextBox");
        TextBox prdnum = (TextBox)FormView1.FindControl("prdnumTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "addnewrec";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmCompany"].DefaultValue = company.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfid"].DefaultValue = fid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvname"].DefaultValue = name.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsid"].DefaultValue = statid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaddr1"].DefaultValue = add1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaddr2"].DefaultValue = add2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcity"].DefaultValue = city.Text.Trim();
        ObjectDataSource1.SelectParameters["prmstate"].DefaultValue = stat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmzip"].DefaultValue = zip.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcoacc"].DefaultValue = accomp.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnumper"].DefaultValue = numper.Text.Trim();
        ObjectDataSource1.SelectParameters["prmacclevel"].DefaultValue = acclevel.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaccdig1"].DefaultValue = acc_dig1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaccdig2"].DefaultValue = acc_dig2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaccdig3"].DefaultValue = acc_dig3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaccdig4"].DefaultValue = acc_dig4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmaccdig5"].DefaultValue = acc_dig5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmyendoff"].DefaultValue = yendoff.Text.Trim();

        ObjectDataSource1.SelectParameters["prmcurrcode"].DefaultValue = currcode.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmyendper"].DefaultValue = terr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfirstyear"].DefaultValue = firstyear.Text.Trim();
        ObjectDataSource1.SelectParameters["prmprdnum"].DefaultValue = prdnum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmprddt1"].DefaultValue = prddt1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmprddt2"].DefaultValue = prddt2.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
       

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        Label company = (Label)FormView1.FindControl("companyLabel");
        TextBox fid = (TextBox)FormView1.FindControl("fidTextBox");
        TextBox name = (TextBox)FormView1.FindControl("vnameTextBox");
        TextBox statid = (TextBox)FormView1.FindControl("sidTextBox");
        TextBox add1 = (TextBox)FormView1.FindControl("addr1TextBox");
        TextBox add2 = (TextBox)FormView1.FindControl("addr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("cityTextBox");
        TextBox stat = (TextBox)FormView1.FindControl("stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("zipTextBox");
        TextBox accomp = (TextBox)FormView1.FindControl("co_accTextBox");

        TextBox numper = (TextBox)FormView1.FindControl("num_perTextBox");
        TextBox acclevel = (TextBox)FormView1.FindControl("acc_levelTextBox");
        TextBox acc_dig1 = (TextBox)FormView1.FindControl("acc_dig1TextBox");
        TextBox acc_dig2 = (TextBox)FormView1.FindControl("acc_dig2TextBox");
        TextBox acc_dig3 = (TextBox)FormView1.FindControl("acc_dig3TextBox");
        TextBox acc_dig4 = (TextBox)FormView1.FindControl("acc_dig4TextBox");
        TextBox acc_dig5 = (TextBox)FormView1.FindControl("acc_dig5TextBox");

        TextBox yendoff = (TextBox)FormView1.FindControl("yend_offTextBox");
        TextBox firstyear = (TextBox)FormView1.FindControl("firstyearTextBox");
        TextBox prddt1 = (TextBox)FormView1.FindControl("prddt1TextBox");
        TextBox prddt2 = (TextBox)FormView1.FindControl("prddt2TextBox");
        TextBox currcode = (TextBox)FormView1.FindControl("curr_codeTextBox");
        TextBox prdnum = (TextBox)FormView1.FindControl("prdnumTextBox");
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        try
        {

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "updaterec";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmCompany"].DefaultValue = company.Text.Trim();
            ObjectDataSource1.SelectParameters["prmfid"].DefaultValue = fid.Text.Trim();
            ObjectDataSource1.SelectParameters["prmvname"].DefaultValue = name.Text.Trim();
            ObjectDataSource1.SelectParameters["prmsid"].DefaultValue = statid.Text.Trim();
            ObjectDataSource1.SelectParameters["prmaddr1"].DefaultValue = add1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmaddr2"].DefaultValue = add2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmcity"].DefaultValue = city.Text.Trim();
            ObjectDataSource1.SelectParameters["prmstate"].DefaultValue = stat.Text.Trim();
            ObjectDataSource1.SelectParameters["prmzip"].DefaultValue = zip.Text.Trim();
            ObjectDataSource1.SelectParameters["prmcoacc"].DefaultValue = accomp.Text.Trim();
            ObjectDataSource1.SelectParameters["prmnumper"].DefaultValue = numper.Text.Trim();
            ObjectDataSource1.SelectParameters["prmacclevel"].DefaultValue = acclevel.Text.Trim();
            ObjectDataSource1.SelectParameters["prmaccdig1"].DefaultValue = acc_dig1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmaccdig2"].DefaultValue = acc_dig2.Text.Trim();
            ObjectDataSource1.SelectParameters["prmaccdig3"].DefaultValue = acc_dig3.Text.Trim();
            ObjectDataSource1.SelectParameters["prmaccdig4"].DefaultValue = acc_dig4.Text.Trim();
            ObjectDataSource1.SelectParameters["prmaccdig5"].DefaultValue = acc_dig5.Text.Trim();
            ObjectDataSource1.SelectParameters["prmyendoff"].DefaultValue = yendoff.Text.Trim();

            ObjectDataSource1.SelectParameters["prmcurrcode"].DefaultValue = currcode.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmyendper"].DefaultValue = terr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmfirstyear"].DefaultValue = firstyear.Text.Trim();
            ObjectDataSource1.SelectParameters["prmprdnum"].DefaultValue = prdnum.Text.Trim();
            ObjectDataSource1.SelectParameters["prmprddt1"].DefaultValue = prddt1.Text.Trim();
            ObjectDataSource1.SelectParameters["prmprddt2"].DefaultValue = prddt2.Text.Trim();
            FormView1.ChangeMode(FormViewMode.ReadOnly);
                 
            
        }
        catch { }
            
           
       


    }


    protected void Delete_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        Session["customer1_list_index"] = null;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
       
       /* Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");*/
    }
    protected void lnk_listperiod_Click(object sender, EventArgs e)
    {
        Response.Redirect("period_list.aspx");
    }
    protected void lnk_viewperiod_Click(object sender, EventArgs e)
    {
        Response.Redirect("period_view.aspx");
    }
    
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }



}
