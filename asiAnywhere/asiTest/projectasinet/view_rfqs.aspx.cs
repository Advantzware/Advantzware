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
using ASINET1;
using ASIDataNS;
using Progress.Open4GL.Proxy;


/// <summary>
/// Summary description for Class1
/// </summary>
public partial class viewrfqs : System.Web.UI.Page
{
    string userid = "";
    public viewrfqs()
    {

    }
    protected void Page_Load(object sender, EventArgs e)
    {
        
        //FormView1.Visible = true;
        //FormView1.ChangeMode(FormViewMode.ReadOnly);
        //ImageButton viewrfq = (ImageButton)Master.FindControl("view_rfqs");
        //viewrfq.ImageUrl = "~/Images/view RFQ1.jpg";
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "View Rfqs";
        //FormView1.ChangeMode(FormViewMode.ReadOnly);

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "";

        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmExt"].DefaultValue = "";
        //ObjectDataSource1.SelectParameters["prmRfqNo"].DefaultValue = Convert.ToString(Session["Rfqview"]);


        try
        {
            Label rfq = (Label)FormView1.FindControl("Label1");
            Session["Rfqview"] = rfq.Text;
            //Session["Rfqview"] = Session["get_new_rfq_no_2"];
        }
        catch { }
        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "view_rfqs.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;
                func1 f1 = new func1();
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                userid = aUsers;
                if (aUsers == "external")
                {                    
                    try
                    {
                        Label commlabel = (Label)FormView1.FindControl("commlabel");
                        Label comm = (Label)FormView1.FindControl("Label15");
                        comm.Visible = false;
                        commlabel.Visible = false;                        
                    }
                    catch
                    {
                    }
                 
                }
                if (vCanRun == true)
                {

                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");
                }
                if (FormView1.DataItemCount == 0)
                {

                    newButton.Visible = true;
                }
                else
                {
                    newButton.Visible = false;
                }
                if (Convert.ToString(Session["add_rfq_list_buton"]) == "add")
                {
                    newButton.Visible = false;
                    if (userid == "external")
                    {                        
                        Session["Rfqview"] = Session["rfqsnos"];

                        FormView1.Visible = true;                        
                        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                        ObjectDataSource2.SelectParameters["prmExt"].DefaultValue = "";                        
                        FormView1.ChangeMode(FormViewMode.Insert);
                    }
                    if (userid == "internal")
                    {
                        FormView1.ChangeMode(FormViewMode.Insert);
                    }
                    Session["add_rfq_list_buton"] = null;
                }

            }                        
        }
    }
    protected void newbutton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        newButton.Visible = false;
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.ReadOnly)
            {
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_rfqs.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    if (aUsers == "external")
                    {
                        Label commlabel = (Label)FormView1.FindControl("commlabel");
                        Label comm = (Label)FormView1.FindControl("Label15");
                        comm.Visible = false;
                        commlabel.Visible = false;
                    }
                    
                }
            }

            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                Image image4 = (Image)FormView1.FindControl("Image4");
                image4.Visible = false;
                RadioButtonList rdo1 = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
                rdo1.SelectedIndex = 0;
                Label rfq = (Label)FormView1.FindControl("rfq_noTextBox");
                DropDownList chg = (DropDownList)FormView1.FindControl("chg_methodTextBox");              
                chg.SelectedIndex = 2;

                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                try
                {
                    rfqs rfqs = new rfqs();
                    DataSet dsrfqs = new DataSet();

                    dsrfqs = rfqs.ViewRfq(UserLogin.UserName, "getRfqNo", "", 0, Convert.ToDateTime("12/12/2011"), Convert.ToDateTime("12/12/2011"), "", "", "", "", "", "", "", "", "", 0, "", "", 0, "", 0);

                    rfq.Text = dsrfqs.Tables[0].Rows[0][0].ToString();
                }
                catch { }

                TextBox reqdate = (TextBox)FormView1.FindControl("req_dateTextBox");
                TextBox duedate = (TextBox)FormView1.FindControl("due_dateTextBox");

                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_rfqs.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    if (aUsers == "external")
                    {
                        Label commlabel = (Label)FormView1.FindControl("commlabel");
                        TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
                        Image imagecustlook = (Image)FormView1.FindControl("CustomerLook");

                        TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");
                        TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
                        TextBox addr1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
                        TextBox addr2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
                        TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
                        TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
                        TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
                        TextBox salesman = (TextBox)FormView1.FindControl("smanTextBox");
                        TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
                        

                        comm.Visible = false;
                        commlabel.Visible = false;
                        image4.Visible = true;
                        imagecustlook.Visible = false;              
                       
                        TextBox sinst = (TextBox)FormView1.FindControl("instTextBox");
                        TextBox fobcod = (TextBox)FormView1.FindControl("fob_codeTextBox");

                        try
                        {

                        Order ord = new Order();
                        DataSet ds = new DataSet();
                        
                        ds = ord.SelectCustomer("select", UserLogin.UserName, "cust-no", "EQUAL", "");                        

                        cust.Text = ds.Tables[0].Rows[0][0].ToString();
                        custname.Text = ds.Tables[0].Rows[0][1].ToString();
                        addr1.Text = ds.Tables[0].Rows[0][2].ToString();
                        addr2.Text = ds.Tables[0].Rows[0][3].ToString();
                        city.Text = ds.Tables[0].Rows[0][4].ToString();
                        state.Text = ds.Tables[0].Rows[0][5].ToString();
                        zip.Text = ds.Tables[0].Rows[0][6].ToString();

                        salesman.Text = ds.Tables[0].Rows[0][8].ToString();
                        //HiddenFieldsman.Value = ds.Tables[0].Rows[0][8].ToString();
                        sname.Text = ds.Tables[0].Rows[0][9].ToString();
                        //HiddenFieldsname.Value = ds.Tables[0].Rows[0][9].ToString();
                        fobcod.Text = ds.Tables[0].Rows[0][16].ToString();
                        //HiddenFieldComm.Value = ds.Tables[0].Rows[0][15].ToString();                     
                        }
                        catch
                        {
                            HttpContext.Current.Response.Write("<script>alert('Invalid Customer Name')</script>");
                        }


                        

                    }
                    if (aUsers == "internal")
                    {
                        TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
                        comm.Enabled = false;                                                                       
                    }

                    reqdate.Text = DateTime.Now.ToShortDateString();
                    duedate.Text = DateTime.Now.ToShortDateString();
                    reqdate.Focus();

                }
            }


            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {
                    string vUserId = UserLogin.UserName;
                    string vPage = "view_rfqs.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();

                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    if (aUsers == "external")
                    {
                        TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");
                        TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
                        TextBox addr1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
                        TextBox addr2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
                        TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
                        TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
                        TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
                        TextBox salesman = (TextBox)FormView1.FindControl("smanTextBox");
                        TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
                        Label commlabel = (Label)FormView1.FindControl("commlabel");
                        TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
                        comm.Visible = false;
                        commlabel.Visible = false;

                        RadioButton fob1 = (RadioButton)FormView1.FindControl("RD5");
                        RadioButton fob2 = (RadioButton)FormView1.FindControl("RD6");
                        DropDownList fcharge = (DropDownList)FormView1.FindControl("chg_methodTextBox");
                        Image salesrep = (Image)FormView1.FindControl("SalesRep");
                        salesrep.Visible = false;
                        Image custlook = (Image)FormView1.FindControl("CustomerLook");
                        custlook.Visible = false;

                        cust.Enabled = false;
                        custname.Enabled = false;
                        addr1.Enabled = false;
                        addr2.Enabled = false;
                        city.Enabled = false;
                        state.Enabled = false;
                        sname.Enabled = false;
                        zip.Enabled = false;
                        salesman.Enabled = false;
                        fob1.Enabled = false;
                        fob2.Enabled = false;
                        fcharge.Enabled = false;
                    }
                    if (aUsers == "internal")
                    {
                        TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
                        HiddenFieldComm.Value = comm.Text;
                        comm.Enabled = false;

                    }
                }
            }

            TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
            RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
            RadioButton origin = (RadioButton)FormView1.FindControl("RD6");
            TextBox reqdate2 = (TextBox)FormView1.FindControl("req_dateTextBox");
            reqdate2.Focus();
            if (fob.Text == "D" || fob.Text=="DEST")
            {
                destination.Checked = true;
            }
            else
            {
                origin.Checked = true;
            }
            if (fob.Text == "O")
            {
                origin.Checked = true;
            }
            else
            {
                destination.Checked = true;
            }
        }
        catch { return; }

    }

    protected void UpdateButon_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_rfqs.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

        }

        Label rfq = (Label)FormView1.FindControl("rfq_noTextBox");
        //Response.Write(rfq.Text);
        Session["Rfqview"] = rfq.Text;
        TextBox requesteddate = (TextBox)FormView1.FindControl("req_dateTextBox");
        TextBox duedate = (TextBox)FormView1.FindControl("due_dateTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");

        TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
        DropDownList freightcharge = (DropDownList)FormView1.FindControl("chg_methodTextBox");
        TextBox address1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
        TextBox address2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
        DropDownList warehouse = (DropDownList)FormView1.FindControl("wh_monthTextBox");
        TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
        TextBox commision = (TextBox)FormView1.FindControl("commTextBox");
        TextBox sinst = (TextBox)FormView1.FindControl("instTextBox");
        TextBox sno = (TextBox)FormView1.FindControl("smanTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
        TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
        RadioButton destination = (RadioButton)FormView1.FindControl("RD5");
        RadioButton origin = (RadioButton)FormView1.FindControl("RD6");
        if (destination.Checked == true)
        {
            fob.Text = "D";
        }
        else
        {
            fob.Text = "O";
        }

        Session["rfqcust"] = cust.Text;

        try
        {

            rfqs rfqs = new rfqs();
            bool check = rfqs.ViewRfqValidate(UserLogin.UserName, "ValidateRfq", "", Convert.ToInt32(rfq.Text.Trim()), Convert.ToDateTime("01/01/0001"), Convert.ToDateTime("01/01/0001"), cust.Text.Trim(), "", "", "", "", "", "", sno.Text.Trim(), sname.Text.Trim(), 0, "", "", 0, "", 0);

            if (check)
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "UpdateRfqView";
                ObjectDataSource1.SelectParameters["PrmRfqNo"].DefaultValue = rfq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmReqdate"].DefaultValue = requesteddate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDuedate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCustno"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipname"].DefaultValue = custname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipAddr"].DefaultValue = address1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipAddr2"].DefaultValue = address2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipcity"].DefaultValue = city.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipstate"].DefaultValue = state.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipzip"].DefaultValue = zip.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sno.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSmanName"].DefaultValue = sname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmComm"].DefaultValue = HiddenFieldComm.Value;
                ObjectDataSource1.SelectParameters["prmFobcode"].DefaultValue = fob.Text.Trim();
                ObjectDataSource1.SelectParameters["prmChgmethod"].DefaultValue = freightcharge.Text.Trim();
                ObjectDataSource1.SelectParameters["prmWhmonth"].DefaultValue = warehouse.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmInst"].DefaultValue = sinst.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
        }
        catch { }
    }
   

    protected void Delete_Rfqview(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
                
        //ObjectDataSource1.SelectParameters["vRowid"].DefaultValue = swhere;
        Label rfq = (Label)FormView1.FindControl("Label1");
               
        ObjectDataSource1.SelectParameters["PrmRfqNo"].DefaultValue = rfq.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DeleteView";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

        //Session["delete"] = "Delete";
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        

    }
    

    protected void InsertButton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        TextBox reqdate = (TextBox)FormView1.FindControl("req_dateTextBox");
        TextBox duedate = (TextBox)FormView1.FindControl("due_dateTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");
        TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
        TextBox addr1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
        TextBox addr2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
        TextBox sman = (TextBox)FormView1.FindControl("smanTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
        TextBox sinst = (TextBox)FormView1.FindControl("instTextBox");
        TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
        TextBox comm = (TextBox)FormView1.FindControl("commTextBox");
        DropDownList fchrg = (DropDownList)FormView1.FindControl("chg_methodTextBox");
        DropDownList whmonth = (DropDownList)FormView1.FindControl("wh_monthTextBox");        
        RadioButtonList rdo1 = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        Label rfq = (Label)FormView1.FindControl("rfq_noTextBox");
        if (rdo1.SelectedIndex == 0)
        {
            fob.Text = "D";
        }
        else
        {
            fob.Text = "O";
        }

        Session["rfqcust"] = cust.Text;

        try
        {
            rfqs rfqs = new rfqs();
            bool check = rfqs.ViewRfqValidate(UserLogin.UserName, "ValidateRfq", "", Convert.ToInt32(Session["Rfqview"]), Convert.ToDateTime("01/01/0001"), Convert.ToDateTime("01/01/0001"), cust.Text.Trim(), "", "", "", "", "", "", sman.Text.Trim(), sname.Text.Trim(), 0, "", "", 0, "", 0);
            if (check)
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "AddRfqView";
                ObjectDataSource1.SelectParameters["PrmRfqNo"].DefaultValue = rfq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmReqdate"].DefaultValue = reqdate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDuedate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCustno"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipname"].DefaultValue = custname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipAddr"].DefaultValue = addr1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipAddr2"].DefaultValue = addr2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipcity"].DefaultValue = city.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipstate"].DefaultValue = state.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipzip"].DefaultValue = zip.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSman"].DefaultValue = sman.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSmanName"].DefaultValue = sname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmInst"].DefaultValue = sinst.Text.Trim();
                ObjectDataSource1.SelectParameters["prmComm"].DefaultValue = HiddenFieldComm.Value;
                ObjectDataSource1.SelectParameters["prmFobcode"].DefaultValue = fob.Text.Trim();
                ObjectDataSource1.SelectParameters["prmChgmethod"].DefaultValue = fchrg.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmWhmonth"].DefaultValue = whmonth.SelectedValue.Trim();

                Session["Rfqview"] = rfq.Text.Trim();
                Session["Rfqseq"] = Session["Rfqview"];
                Session["rfqsnos"] = Session["Rfqview"];
                //Response.Write("<script>window.location.href='list_rfqs.aspx'</script>");
                FormView1.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>javascript:window.location.href='rfqitem.aspx?rfqitmmod=insert'</script>");
            }
        }
        catch { }
    }
   
    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label rfqno = (Label)FormView1.FindControl("Label1");
            Session["my_new_rfq"] = rfqno.Text;
            if (Session["Rfqview"] == Session["my_new_rfq"])
            {
                Session["my_new_rfq"] = null;
            }
            else
            {
                Session["Rfqseq"] = Session["my_new_rfq"];
                Session["Rfqview"] = Session["my_new_rfq"];

                Session["rfqsnos"] = Session["my_new_rfq"];
                //Session["list_rfq_rfq_nos"] = Session["my_new_rfq"];
            }
        }
        catch {  }
    }

    

   
    protected void Update2cancle_click(object sender, EventArgs e)
    {
        //FormView2.Visible = false;
        FormView1.Visible = true;
    }
    
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        if (Session["get_new_rfq_no_2"] != null)
        {
            Session["Rfqview"] = Session["get_new_rfq_no_2"];
        }
        try
        {
            Label ldest = (Label)FormView1.FindControl("Label5");
            RadioButton dest = (RadioButton)FormView1.FindControl("RD5");
            RadioButton orig = (RadioButton)FormView1.FindControl("RD6");
            if (ldest.Text == "D" || ldest.Text=="DEST")
            {
                dest.Checked = true;
            }
            if (ldest.Text == "O")
            {
                orig.Checked = true;
            }
           // Response.Write(ldest.Text);
        }
        catch { }
    }
    protected void F1UpdateButton_Click(object sender, EventArgs e)
    {
        //FormView2.ChangeMode(FormViewMode.Insert);
    }
    protected void UpdateButton_Cancel_Click(object sender, EventArgs e)
    {
        Label rfq = (Label)FormView1.FindControl("rfq_noTextBox");
        Session["Rfqview"] = rfq.Text;
    }    

    protected void cust_change_textbox(object sender, EventArgs e)
    {        
        TextBox cust = (TextBox)FormView1.FindControl("cust_noTextBox");
        TextBox custname = (TextBox)FormView1.FindControl("ship_nameTextBox");
        TextBox addr1 = (TextBox)FormView1.FindControl("shipAddrTextBox");
        TextBox addr2 = (TextBox)FormView1.FindControl("shipAddr2TextBox");
        TextBox city = (TextBox)FormView1.FindControl("ship_cityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("ship_stateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("ship_zipTextBox");
        TextBox sman = (TextBox)FormView1.FindControl("smanTextBox");
        TextBox sname = (TextBox)FormView1.FindControl("smanNameTextBox");
        TextBox sinst = (TextBox)FormView1.FindControl("instTextBox");
        TextBox fob = (TextBox)FormView1.FindControl("fob_codeTextBox");
        TextBox comm = (TextBox)FormView1.FindControl("commTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        try
        {
            Order ord = new Order();            
            DataSet ds = new DataSet();

            ds = ord.SelectCustomer("search", UserLogin.UserName, "cust-no", "EQUAL", cust.Text.Trim());         

            custname.Text = ds.Tables[0].Rows[0][1].ToString();
            addr1.Text = ds.Tables[0].Rows[0][2].ToString();
            addr2.Text = ds.Tables[0].Rows[0][3].ToString();
            city.Text = ds.Tables[0].Rows[0][4].ToString();
            state.Text = ds.Tables[0].Rows[0][5].ToString();
            zip.Text = ds.Tables[0].Rows[0][6].ToString();

            sman.Text = ds.Tables[0].Rows[0][8].ToString();
            sname.Text = ds.Tables[0].Rows[0][9].ToString();
            fob.Text = ds.Tables[0].Rows[0][16].ToString();
            comm.Text = ds.Tables[0].Rows[0][15].ToString();
            HiddenFieldComm.Value = ds.Tables[0].Rows[0][15].ToString();
            cust.Focus();
        }
        catch
        {
            HttpContext.Current.Response.Write("<script>alert('Invalid Customer Name')</script>");
        }
        
    }
    
}
